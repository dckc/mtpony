// ack: https://stackoverflow.com/a/29500100/7963 Johan Wikström Apr 2015

package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"strings"
	"unicode"
)

// Reads all .txt files in the current folder
// and encodes them as strings literals in textfiles.go
func main() {
	ext := os.Args[1]
	outName := os.Args[2]
	pkgName := os.Args[3]
	log.Printf("output: %q package: %q extension; %q\n", outName, pkgName, ext)

	fs, _ := ioutil.ReadDir(".")
	out, _ := os.Create(outName + ".go")
	out.Write([]byte("package " + pkgName + "\n\n"))
	for _, f := range fs {
		if strings.HasSuffix(f.Name(), ext) {
			log.Printf("found: %q\n", f.Name())
			out.Write([]byte("var " + strings.TrimSuffix(f.Name(), ext) + " = []byte{\n\t"))
			content, _ := ioutil.ReadFile(f.Name())
			for ix, b := range content {
				if b < 128 && unicode.IsGraphic(rune(b)) {
					out.Write([]byte(fmt.Sprintf("'%c', ", b)))
				} else {
					out.Write([]byte(fmt.Sprintf("0x%02x, ", b)))
				}
				if (ix + 1) % 12 == 0 {
					out.Write([]byte("\n\t"))
				}
			}
			out.Write([]byte("\n}\n"))
		}
	}
}
