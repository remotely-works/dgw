// go:generate go-bindata -o bindata.go template mapconfig
package main

import (
	"bytes"
	"database/sql"
	"fmt"
	"go/format"
	"io/ioutil"
	"log"
	"regexp"
	"sort"
	"strings"
	"text/template"
	"unicode"

	"github.com/iancoleman/strcase"
	"github.com/lib/pq"

	"github.com/BurntSushi/toml"
	_ "github.com/lib/pq" // postgres
	"github.com/pkg/errors"
)

// Queryer database/sql compatible query interface
type Queryer interface {
	Exec(string, ...interface{}) (sql.Result, error)
	Query(string, ...interface{}) (*sql.Rows, error)
	QueryRow(string, ...interface{}) *sql.Row
}

// OpenDB opens database connection
func OpenDB(connStr string) (*sql.DB, error) {
	conn, err := sql.Open("postgres", connStr)
	if err != nil {
		return nil, errors.Wrap(err, "failed to connect to database")
	}
	return conn, nil
}

const pgLoadEnumDef = `
SELECT n.nspname AS schema,
       pg_catalog.format_type ( t.oid, NULL ) AS name,
       ARRAY( SELECT e.enumlabel
                  FROM pg_catalog.pg_enum e
                  WHERE e.enumtypid = t.oid
                  ORDER BY e.oid )
         AS elements
FROM pg_catalog.pg_type t
       LEFT JOIN pg_catalog.pg_namespace n
                 ON n.oid = t.typnamespace
WHERE ( t.typrelid = 0
  OR ( SELECT c.relkind = 'c'
       FROM pg_catalog.pg_class c
       WHERE c.oid = t.typrelid
        )
  )
  AND NOT EXISTS
  ( SELECT 1
    FROM pg_catalog.pg_type el
    WHERE el.oid = t.typelem
      AND el.typarray = t.oid
  )
  AND n.nspname = $1
  AND pg_catalog.pg_type_is_visible ( t.oid )
ORDER BY 1, 2;
`

const queryInterface = `
// Queryer database/sql compatible query interface
type Queryer interface {
	Exec(string, ...interface{}) (sql.Result, error)
	Query(string, ...interface{}) (*sql.Rows, error)
	QueryRow(string, ...interface{}) *sql.Row
}
`

const pgLoadColumnDef = `
SELECT
    a.attnum AS field_ordinal,
    a.attname AS column_name,
    format_type(a.atttypid, a.atttypmod) AS data_type,
    a.attnotnull AS not_null,
    COALESCE(pg_get_expr(ad.adbin, ad.adrelid), '') AS default_value,
    COALESCE(ct.contype = 'p', false) AS  is_primary_key,
    CASE
        WHEN a.atttypid = ANY ('{int,int8,int2}'::regtype[])
          AND EXISTS (
             SELECT 1 FROM pg_attrdef ad
             WHERE  ad.adrelid = a.attrelid
             AND    ad.adnum   = a.attnum
             AND    pg_get_expr(ad.adbin, ad.adrelid) = 'nextval('''
                || (pg_get_serial_sequence (a.attrelid::regclass::text
                                          , a.attname))::regclass
                || '''::regclass)'
             )
            THEN CASE a.atttypid
                    WHEN 'int'::regtype  THEN 'serial'
                    WHEN 'int8'::regtype THEN 'bigserial'
                    WHEN 'int2'::regtype THEN 'smallserial'
                 END
        WHEN a.atttypid = ANY ('{uuid}'::regtype[]) AND COALESCE(pg_get_expr(ad.adbin, ad.adrelid), '') != ''
            THEN 'autogenuuid'
        ELSE format_type(a.atttypid, a.atttypmod)
    END AS data_type
FROM pg_attribute a
JOIN ONLY pg_class c ON c.oid = a.attrelid
JOIN ONLY pg_namespace n ON n.oid = c.relnamespace
LEFT JOIN pg_constraint ct ON ct.conrelid = c.oid
AND a.attnum = ANY(ct.conkey) AND ct.contype = 'p'
LEFT JOIN pg_attrdef ad ON ad.adrelid = c.oid AND ad.adnum = a.attnum
WHERE a.attisdropped = false
AND n.nspname = $1
AND c.relname = $2
AND a.attnum > 0
ORDER BY a.attnum
`

const pgLoadTableDef = `
SELECT
c.relkind AS type,
c.relname AS table_name
FROM pg_class c
JOIN ONLY pg_namespace n ON n.oid = c.relnamespace
WHERE n.nspname = $1
AND c.relkind = 'r'
ORDER BY c.relname
`

const pgLoadColumnConstraint = `
SELECT pg_get_constraintdef(ct.oid) as condef
FROM pg_attribute a
JOIN ONLY pg_class c ON c.oid = a.attrelid
JOIN ONLY pg_namespace n ON n.oid = c.relnamespace
LEFT JOIN pg_constraint ct ON  a.attnum = ANY(ct.conkey) AND  ct.conrelid = c.oid
	AND ct.contype = $4
WHERE
n.nspname = $1
AND c.relname = $2
AND a.attname = $3
`

// TypeMap go/db type map struct
type TypeMap struct {
	Tables         []string `toml:"tables"`
	DBTypes        []string `toml:"db_types"`
	NotNullGoType  string   `toml:"notnull_go_type"`
	NullableGoType string   `toml:"nullable_go_type"`

	compiled   bool
	rePatterns []*regexp.Regexp
}

func (t *TypeMap) Match(table string, s string) bool {
	if !t.compiled {
		for _, v := range t.DBTypes {
			if strings.HasPrefix(v, "re/") {
				t.rePatterns = append(t.rePatterns, regexp.MustCompile(v[3:]))
			}
		}
	}

	if len(t.Tables) != 0 && !contains(table, t.Tables) {
		return false
	}

	if contains(s, t.DBTypes) {
		return true
	}

	for _, v := range t.rePatterns {
		if v.MatchString(s) {
			return true
		}
	}

	return false
}

// AutoKeyMap auto generating key config
type AutoKeyMap struct {
	Types []string `toml:"db_types"`
}

// PgTypeMapConfig go/db type map struct toml config
type PgTypeMapConfig map[string]*TypeMap

// PgTable postgres table
type PgTable struct {
	Schema      string
	Name        string
	DataType    string
	AutoGenPk   bool
	PrimaryKeys []*PgColumn
	Columns     []*PgColumn
}

var autoGenKeyCfg = &AutoKeyMap{
	Types: []string{"smallserial", "serial", "bigserial", "autogenuuid"},
}

func (t *PgTable) setPrimaryKeyInfo(cfg *AutoKeyMap) {
	t.AutoGenPk = false
	for _, c := range t.Columns {
		if c.IsPrimaryKey {
			t.PrimaryKeys = append(t.PrimaryKeys, c)
			for _, typ := range cfg.Types {
				if c.DDLType == typ {
					t.AutoGenPk = true
				}
			}
		}
	}
	return
}

// PgColumn postgres columns
type PgColumn struct {
	FieldOrdinal int
	Name         string
	DataType     string
	DDLType      string
	NotNull      bool
	DefaultValue sql.NullString
	IsPrimaryKey bool
	IsUnique     bool
}

// Struct go struct
type Struct struct {
	Name    string
	Table   *PgTable
	Comment string
	Fields  []*StructField
}

// StructTmpl go struct passed to template
type StructTmpl struct {
	Struct *Struct
}

// StructField go struct field
type StructField struct {
	Name   string
	Type   string
	Tag    string
	Column *PgColumn
}

// PgLoadTypeMapFromFile load type map from toml file
func PgLoadTypeMapFromFile(filePath string) (*PgTypeMapConfig, error) {
	var conf PgTypeMapConfig
	if _, err := toml.DecodeFile(filePath, &conf); err != nil {
		return nil, errors.Wrap(err, "faild to parse config file")
	}
	return &conf, nil
}

type PgEnum struct {
	Schema string
	Name   string
	Values []string
}

type EnumValue struct {
	Type  *EnumType
	Name  string
	Value string
}

type EnumType struct {
	Name    string
	Enum    *PgEnum
	Comment string
	Values  []EnumValue
}

func PgLoadEnumDef(db Queryer, schema string) ([]*PgEnum, error) {
	enumDefs, err := db.Query(pgLoadEnumDef, schema)
	if err != nil {
		return nil, errors.Wrap(err, "failed to load enum def")
	}

	enums := []*PgEnum{}
	for enumDefs.Next() {
		e := &PgEnum{}
		var vals pq.StringArray
		err := enumDefs.Scan(
			&e.Schema,
			&e.Name,
			&vals,
		)
		e.Values = vals
		if err != nil {
			return nil, errors.Wrap(err, "failed to scan")
		}

		e.Name = strings.Replace(e.Name, `"`, "", -1)
		enums = append(enums, e)
	}
	return enums, nil
}

// PgLoadColumnDef load Postgres column definition
func PgLoadColumnDef(db Queryer, schema string, table string) ([]*PgColumn, error) {
	colDefs, err := db.Query(pgLoadColumnDef, schema, table)
	if err != nil {
		return nil, errors.Wrap(err, "failed to load table def")
	}

	cols := []*PgColumn{}
	for colDefs.Next() {
		c := &PgColumn{}
		err := colDefs.Scan(
			&c.FieldOrdinal,
			&c.Name,
			&c.DataType,
			&c.NotNull,
			&c.DefaultValue,
			&c.IsPrimaryKey,
			&c.DDLType,
		)
		if err != nil {
			return nil, errors.Wrap(err, "failed to scan")
		}

		c.DataType = strings.Replace(c.DataType, `"`, "", -1)
		// Some data types have an extra part e.g, "character varying(16)" and
		// "numeric(10, 5)". We want to drop the extra part.
		if i := strings.Index(c.DataType, "("); i > 0 {
			c.DataType = c.DataType[0:i]
		}

		cols = append(cols, c)
	}
	return cols, nil
}

// PgLoadTableDef load Postgres table definition
func PgLoadTableDef(db Queryer, schema string) ([]*PgTable, error) {
	tbDefs, err := db.Query(pgLoadTableDef, schema)
	if err != nil {
		return nil, errors.Wrap(err, "failed to load table def")
	}
	tbs := []*PgTable{}
	for tbDefs.Next() {
		t := &PgTable{Schema: schema}
		err := tbDefs.Scan(
			&t.DataType,
			&t.Name,
		)
		if err != nil {
			return nil, errors.Wrap(err, "failed to scan")
		}
		cols, err := PgLoadColumnDef(db, schema, t.Name)
		if err != nil {
			return nil, errors.Wrap(err, fmt.Sprintf("failed to get columns of %s", t.Name))
		}
		t.Columns = cols
		tbs = append(tbs, t)
	}
	return tbs, nil
}

func contains(v string, l []string) bool {
	sort.Strings(l)
	i := sort.SearchStrings(l, v)
	if i < len(l) && l[i] == v {
		return true
	}
	return false
}

// PgConvertType converts type
func PgConvertType(t *PgTable, col *PgColumn, typeCfg PgTypeMapConfig) string {
	typ := typeCfg["default"].NotNullGoType
	for _, v := range typeCfg {
		if v.Tables == nil {
			continue
		}

		if v.Match(t.Name, col.DataType) {
			if col.NotNull {
				return v.NotNullGoType
			}
			return v.NullableGoType
		}
	}

	for _, v := range typeCfg {
		if v.Tables != nil {
			continue
		}

		if v.Match(t.Name, col.DataType) {
			if col.NotNull {
				return v.NotNullGoType
			}
			return v.NullableGoType
		}
	}
	return typ
}

// PgColToField converts pg column to go struct field
func PgColToField(db Queryer, t *PgTable, col *PgColumn, typeCfg PgTypeMapConfig) (*StructField, error) {
	stfType := PgConvertType(t, col, typeCfg)
	stf := &StructField{
		Name:   PublicVarName(col.Name, false),
		Type:   stfType,
		Column: col,
	}
	return fillStructTags(db, stf, t, col)
}

func fillStructTags(db Queryer, st *StructField, t *PgTable, col *PgColumn) (*StructField, error) {
	var tags []string
	if col.IsPrimaryKey {
		tags = append(tags, `pk:"true"`)
	}

	unique, err := isUnique(db, st, t, col)
	if err != nil {
		return nil, err
	}

	if unique {
		tags = append(tags, `unique:"true"`)
	}

	fk, err := hasForeignKey(db, st, t, col)
	if err != nil {
		return nil, err
	}

	if fk != "" {
		tags = append(tags, fmt.Sprintf(`fk:"%s"`, fk))
	}

	var omitEmpty bool
	if strings.Contains(col.DefaultValue.String, "nextval") {
		tags = append(tags, `auto:"true"`)
		omitEmpty = true
	} else {
		if col.DefaultValue.Valid {
			parts := strings.Split(col.DefaultValue.String, "::")
			defaultVal := parts[0]

			if defaultVal != "" {
				omitEmpty = true
				tags = append(tags, fmt.Sprintf(`default:"%s"`, defaultVal))
			}
		}
	}

	if col.DataType == "json" || col.DataType == "jsonb" || col.DataType == "numeric" {
		tags = append(tags, fmt.Sprintf(`type:"%s"`, col.DataType))
	}

	if !col.NotNull {
		omitEmpty = true
	}

	var omit string
	if omitEmpty {
		omit = ",omitempty"
	}

	tags = append([]string{fmt.Sprintf(`db:"%s" json:"%s%s"`, col.Name, col.Name, omit)}, tags...)
	st.Tag = strings.Join(tags, " ")

	return st, nil
}

func isUnique(db Queryer, st *StructField, t *PgTable, col *PgColumn) (bool, error) {
	colDefs, err := db.Query(pgLoadColumnConstraint, t.Schema, t.Name, col.Name, "u")
	if err != nil {
		return false, errors.Wrap(err, "failed to load column constraint")
	}
	defer colDefs.Close()

	var cont sql.NullString
	if colDefs.Next() {
		if err := colDefs.Scan(&cont); err != nil {
			return false, err
		}
	}

	return cont.String != "", nil
}

func hasForeignKey(db Queryer, st *StructField, t *PgTable, col *PgColumn) (string, error) {
	colDefs, err := db.Query(pgLoadColumnConstraint, t.Schema, t.Name, col.Name, "f")
	if err != nil {
		return "", errors.Wrap(err, "failed to load column constraint")
	}
	defer colDefs.Close()

	var cont sql.NullString
	if colDefs.Next() {
		if err := colDefs.Scan(&cont); err != nil {
			return "", err
		}
	}

	parts := strings.Split(cont.String, "REFERENCES ")
	if len(parts) == 1 {
		return "", nil
	}

	parts = strings.Split(parts[1], `)`)
	fk := strings.ReplaceAll(parts[0], `"`, "") + ")"
	return fk, nil
}

// PgTableToStruct converts table def to go struct
func PgTableToStruct(db Queryer, t *PgTable, typeCfg PgTypeMapConfig, keyConfig *AutoKeyMap) (*Struct, error) {
	t.setPrimaryKeyInfo(keyConfig)
	s := &Struct{
		Name:  PublicVarName(t.Name, true),
		Table: t,
	}
	var fs []*StructField
	for _, c := range t.Columns {
		f, err := PgColToField(db, t, c, typeCfg)
		if err != nil {
			return nil, errors.Wrap(err, "failed to convert col to field")
		}
		fs = append(fs, f)
	}
	s.Fields = fs
	return s, nil
}

// PgExecuteDefaultTmpl execute struct template with *Struct
func PgExecuteDefaultTmpl(st interface{}, path string) ([]byte, error) {
	var src []byte
	d, err := Asset(path)
	if err != nil {
		return src, errors.Wrap(err, "failed to load asset")
	}
	tpl, err := template.New("struct").Funcs(tmplFuncMap).Parse(string(d))
	if err != nil {
		return src, errors.Wrap(err, "failed to parse template")
	}
	buf := new(bytes.Buffer)
	if err := tpl.Execute(buf, st); err != nil {
		return src, errors.Wrap(err, fmt.Sprintf("failed to execute template:\n%s", src))
	}
	src, err = format.Source(buf.Bytes())
	if err != nil {
		return src, errors.Wrap(err, fmt.Sprintf("failed to format code:\n%s", src))
	}
	return src, nil
}

// PgExecuteCustomTmpl execute custom template
func PgExecuteCustomTmpl(st interface{}, customTmpl string) ([]byte, error) {
	var src []byte
	tpl, err := template.New("struct").Funcs(tmplFuncMap).Parse(customTmpl)
	if err != nil {
		return src, errors.Wrap(err, "failed to parse template")
	}
	buf := new(bytes.Buffer)
	if err := tpl.Execute(buf, st); err != nil {
		return src, errors.Wrap(err, fmt.Sprintf("failed to execute custom template:\n%s", src))
	}
	src, err = format.Source(buf.Bytes())
	if err != nil {
		return src, errors.Wrap(err, fmt.Sprintf("failed to format code:\n%s", src))
	}
	return src, nil
}

func getPgTypeMapConfig(typeMapPath string) (PgTypeMapConfig, error) {
	cfg := make(PgTypeMapConfig)
	if typeMapPath == "" {
		if _, err := toml.Decode(typeMap, &cfg); err != nil {
			return nil, errors.Wrap(err, "failed to read type map")
		}
	} else {
		if _, err := toml.DecodeFile(typeMapPath, &cfg); err != nil {
			return nil, errors.Wrap(err, fmt.Sprintf("failed to decode type map file %s", typeMapPath))
		}
	}
	return cfg, nil
}

func PgEnumToType(e *PgEnum, typeCfg PgTypeMapConfig, keyConfig *AutoKeyMap) (*EnumType, error) {
	en := &EnumType{
		Name: PublicVarName(e.Name, true),
		Enum: e,
	}
	for _, v := range e.Values {
		en.Values = append(en.Values, EnumValue{
			Type:  en,
			Name:  PublicVarName(v, true) + en.Name,
			Value: v,
		})
	}
	if _, ok := typeCfg[e.Name]; !ok {
		typeCfg[e.Name] = &TypeMap{
			DBTypes:        []string{e.Name},
			NotNullGoType:  en.Name,
			NullableGoType: "Null" + en.Name,

			compiled:   true,
			rePatterns: nil,
		}
	}

	return en, nil
}

func PgCreateEnums(db Queryer, schema string, cfg PgTypeMapConfig, customTmpl string) (map[string][]byte, error) {
	out := make(map[string][]byte, 0)

	enums, err := PgLoadEnumDef(db, schema)
	if err != nil {
		return nil, errors.Wrap(err, "failed to load enum definitions")
	}

	for _, pgEnum := range enums {
		var src []byte
		enum, err := PgEnumToType(pgEnum, cfg, autoGenKeyCfg)
		if err != nil {
			return nil, errors.Wrap(err, "failed to convert enum definition to type")
		}

		if customTmpl != "" {
			tmpl, err := ioutil.ReadFile(customTmpl)
			if err != nil {
				return nil, err
			}
			s, err := PgExecuteCustomTmpl(enum, string(tmpl))
			if err != nil {
				return nil, errors.Wrap(err, "PgExecuteCustomTmpl failed")
			}
			src = append(src, s...)
		} else {
			s, err := PgExecuteDefaultTmpl(enum, "template/enum.tmpl")
			if err != nil {
				return nil, errors.Wrap(err, "failed to execute template")
			}
			src = append(src, s...)
		}

		out[pgEnum.Name] = src
	}
	return out, nil
}

// PgCreateStruct creates struct from given schema
func PgCreateStruct(
	db Queryer, schema string, cfg PgTypeMapConfig, pkgName, customTmpl, customEnumTmpl string, exTbls []string) (map[string][]byte, error) {
	enums, err := PgCreateEnums(db, schema, cfg, customEnumTmpl)
	if err != nil {
		log.Fatal(err)
	}

	out := make(map[string][]byte, 0)

	tbls, err := PgLoadTableDef(db, schema)
	if err != nil {
		return nil, errors.Wrap(err, "failed to load table definitions")
	}

	for _, tbl := range tbls {
		src := []byte(fmt.Sprintf("package %s\n\n", pkgName))

		if contains(tbl.Name, exTbls) {
			continue
		}
		st, err := PgTableToStruct(db, tbl, cfg, autoGenKeyCfg)
		if err != nil {
			return nil, errors.Wrap(err, "failed to convert table definition to struct")
		}
		if customTmpl != "" {
			tmpl, err := ioutil.ReadFile(customTmpl)
			if err != nil {
				return nil, err
			}
			s, err := PgExecuteCustomTmpl(&StructTmpl{Struct: st}, string(tmpl))
			if err != nil {
				return nil, errors.Wrap(err, "PgExecuteCustomTmpl failed")
			}
			src = append(src, s...)
		} else {
			s, err := PgExecuteDefaultTmpl(&StructTmpl{Struct: st}, "template/struct.tmpl")
			if err != nil {
				return nil, errors.Wrap(err, "failed to execute template")
			}
			m, err := PgExecuteDefaultTmpl(&StructTmpl{Struct: st}, "template/method.tmpl")
			if err != nil {
				return nil, errors.Wrap(err, "failed to execute template")
			}
			src = append(src, s...)
			src = append(src, m...)
		}

		filename := fmt.Sprintf("%s.go", strcase.ToSnake(st.Name))
		out[filename] = src

		for _, col := range tbl.Columns {
			if enum, ok := enums[col.DataType]; ok {
				out[filename] = append(out[filename], enum...)
				delete(enums, col.DataType)
			}
		}
	}
	return out, nil
}

var commonInitialisms = map[string]bool{
	"API":   true,
	"ASCII": true,
	"CPU":   true,
	"CSS":   true,
	"DNS":   true,
	"EOF":   true,
	"GUID":  true,
	"HTML":  true,
	"HTTP":  true,
	"HTTPS": true,
	"ID":    true,
	"IP":    true,
	"JSON":  true,
	"LHS":   true,
	"QPS":   true,
	"RAM":   true,
	"RHS":   true,
	"RPC":   true,
	"SLA":   true,
	"SMTP":  true,
	"SSH":   true,
	"TLS":   true,
	"TTL":   true,
	"UI":    true,
	"UID":   true,
	"UUID":  true,
	"URI":   true,
	"URL":   true,
	"UTF8":  true,
	"VM":    true,
	"XML":   true,
	"PTO":   true,
}

// PublicVarName formats a string as a public go variable name
func PublicVarName(s string, isTable bool) string {
	name := lintFieldName(s)
	runes := []rune(name)
	for i, c := range runes {
		ok := unicode.IsLetter(c) || unicode.IsDigit(c)
		if i == 0 {
			ok = unicode.IsLetter(c)
		}
		if !ok {
			runes[i] = '_'
		}
	}

	newName := string(runes)

	if strings.Index(newName, "BackAc") != -1 {
		newName = strings.Replace(newName, "BackAc", "BankAc", 1)
	}

	if !isTable {
		return newName
	}

	if strings.Index(newName, "Policies") != -1 {
		newName = strings.Replace(newName, "Policies", "Policy", 1)
	}

	if strings.Index(newName, "Pto") != -1 {
		newName = strings.Replace(newName, "Pto", "PTO", 1)
	}

	if strings.Index(newName, "Kids") == -1 && strings.Index(newName, "Days") == -1 && strings.Index(newName, "Comments") == -1 && strings.Index(newName, "atus") == -1 && strings.Index(newName, "ress") == -1 && name[len(newName)-1] == 's' {
		newName = newName[:len(newName)-1]
	}

	return newName
}

func lintFieldName(name string) string {
	// Fast path for simple cases: "_" and all lowercase.
	if name == "_" {
		return name
	}

	for len(name) > 0 && name[0] == '_' {
		name = name[1:]
	}

	allLower := true
	for _, r := range name {
		if !unicode.IsLower(r) {
			allLower = false
			break
		}
	}
	if allLower {
		runes := []rune(name)
		if u := strings.ToUpper(name); commonInitialisms[u] {
			copy(runes[0:], []rune(u))
		} else {
			runes[0] = unicode.ToUpper(runes[0])
		}
		return string(runes)
	}

	// Split camelCase at any lower->upper transition, and split on underscores.
	// Check each word for common initialisms.
	runes := []rune(name)
	w, i := 0, 0 // index of start of word, scan
	for i+1 <= len(runes) {
		eow := false // whether we hit the end of a word

		if i+1 == len(runes) {
			eow = true
		} else if runes[i+1] == '_' {
			// underscore; shift the remainder forward over any run of underscores
			eow = true
			n := 1
			for i+n+1 < len(runes) && runes[i+n+1] == '_' {
				n++
			}

			// Leave at most one underscore if the underscore is between two digits
			if i+n+1 < len(runes) && unicode.IsDigit(runes[i]) && unicode.IsDigit(runes[i+n+1]) {
				n--
			}

			copy(runes[i+1:], runes[i+n+1:])
			runes = runes[:len(runes)-n]
		} else if unicode.IsLower(runes[i]) && !unicode.IsLower(runes[i+1]) {
			// lower->non-lower
			eow = true
		}
		i++
		if !eow {
			continue
		}

		// [w,i) is a word.
		word := string(runes[w:i])
		if u := strings.ToUpper(word); commonInitialisms[u] {
			// All the common initialisms are ASCII,
			// so we can replace the bytes exactly.
			copy(runes[w:], []rune(u))

		} else if strings.ToLower(word) == word {
			// already all lowercase, and not the first word, so uppercase the first character.
			runes[w] = unicode.ToUpper(runes[w])
		}
		w = i
	}
	return string(runes)
}
