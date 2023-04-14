# Convert The Hit List backups to Markdown

As [The Hit List](http://www.karelia.com/products/the-hit-list/mac.html) seems to be abandonware, I've switched to
[Obsidian](https://obsidian.md/) which keeps everything in markdown files. To migrate my old lists, I wrote this simple
converter that will create a markdown file with a bulleted list for each list from a `.thlbackup`.

To run, you will need SWI-Prolog (tested with 9.0.3, but any recent version should work), then run

    ./convert.pl [path-to-thlbackup]

This will dump the markdown files in `output/` under the current directory, which you can copy into your Obsidian vault.

## See also

* https://github.com/joeybaker/the-hit-list-migration for conversion to Things app
