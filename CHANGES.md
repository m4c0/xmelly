1.0.1
-----
  * 🐞 Fix parsing of CDATA
  * 🐞 Comments are now treated as a space
  * 🐞 Accept tabs and windows newlines as spaces
  * 🐞 Multiple spaces are merged into a single one
  * 🐞 Spaces are trimmed around nodes
  * 🐞 Empty tags no longer fail to parse
  * 🐞 Improved parsing of text (no longer takes quotes/exclamation as tokens)
  * 🐞 Allowing quotes inside text without attempting to parse as string
  * 🏭 Split tokenisation into a different module
  * 🧪 Add cram tests

1.0.0
-----
  * 🚀 First release
