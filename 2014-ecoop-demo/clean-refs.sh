#!/bin/bash
grep -v "url =" My\ Library.bib | grep -v "doi =" | grep -v "isbn =" | grep -v "issn ="> vjovanov-lib.bib
