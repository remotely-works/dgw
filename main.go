package main

import (
	"io"
	"log"
	"os"
	"os/exec"
	"path/filepath"

	"gopkg.in/alecthomas/kingpin.v2"
)

var (
	connStr = kingpin.Arg(
		"conn", "PostgreSQL connection string in URL format").Required().String()
	schema = kingpin.Flag(
		"schema", "PostgreSQL schema name").Default("public").Short('s').String()
	pkgName         = kingpin.Flag("package", "package name").Default("main").Short('p').String()
	typeMapFilePath = kingpin.Flag("typemap", "column type and go type map file path").Short('t').String()
	exTbls          = kingpin.Flag("exclude", "table names to exclude").Short('x').Strings()
	customTmpl      = kingpin.Flag("template", "custom template path").String()
	customEnumTmpl  = kingpin.Flag("enum-template", "custom enum template").String()
	outFile         = kingpin.Flag("output", "output file path").Short('o').String()
)

func main() {
	kingpin.Parse()

	conn, err := OpenDB(*connStr)
	if err != nil {
		log.Fatal(err)
	}

	cfg, err := getPgTypeMapConfig(*typeMapFilePath)
	if err != nil {
		log.Fatal(err)
	}

	st, err := PgCreateStruct(conn, *schema, cfg, *pkgName, *customTmpl, *customEnumTmpl, *exTbls)
	if err != nil {
		log.Fatal(err)
	}

	for filename, code := range st {
		fullpath := filepath.Join(*outFile, filename)
		var out io.Writer

		out, err = os.Create(fullpath)
		if err != nil {
			log.Fatalf("failed to create output file %s: %s", *outFile, err)
		}

		if _, err := out.Write(code); err != nil {
			log.Fatal(err)
		}

		params := []string{"-w", fullpath}
		if err := exec.Command("goimports", params...).Run(); err != nil {
			log.Fatalf("failed to goimports: %s", err)
		}
	}
}
