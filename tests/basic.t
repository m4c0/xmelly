Can we parse a nearly empty file?
  $ ./xmldump.exe <<EOF
  > <?xml version="1.0" ?>
  > <hello></hello>
  > EOF
  hello:

Can we have space before root?
  $ ./xmldump.exe <<EOF
  > <?xml version="1.0" ?>
  > 
  > <hello></hello>
  > EOF
  hello:

Can we parse a two-level deep node?
  $ ./xmldump.exe <<EOF
  > <?xml version="1.0" ?>
  > <hello><world>test</world></hello>
  > EOF
  hello:
    world:
      test

Can we use exclamations?
  $ ./xmldump.exe <<EOF
  > <?xml version="1.0" ?>
  > <hello>!!</hello>
  > EOF
  hello:
    !!

Can we mix spaces in text?
  $ ./xmldump.exe <<EOF
  > <?xml version="1.0" ?>
  > <hello>i   am   groot   !   !</hello>
  > EOF
  hello:
    i am groot ! !

Do we merge consecutive spaces?
  $ ./xmldump.exe <<EOF
  > <?xml version="1.0" ?>
  > <hello>[
  >   world
  > ]</hello>
  > EOF
  hello:
    [ world ]

Do we strip spaces between text?
  $ ./xmldump.exe <<EOF
  > <?xml version="1.0" ?>
  > <hello>
  >   world
  > </hello>
  > EOF
  hello:
    world

Do we strip spaces between tags?
  $ ./xmldump.exe <<EOF
  > <?xml version="1.0" ?>
  > <hello>
  >   <world/>
  > </hello>
  > EOF
  hello:
    world:

Can use use paths in text?
  $ ./xmldump.exe <<EOF
  > <?xml version="1.0" ?>
  > <hello>../relative/path.txt</hello>
  > EOF
  hello:
    ../relative/path.txt

Can we ignore comments?
  $ ./xmldump.exe <<EOF
  > <?xml version="1.0" ?>
  > <hello>[<!-- yeah -->]</hello>
  > EOF
  hello:
    [ ]

Can we ignore comments with dashes?
  $ ./xmldump.exe <<EOF
  > <?xml version="1.0" ?>
  > <hello>[<!-- - yeah - -->]</hello>
  > EOF
  hello:
    [ ]

Can we process empty tags?
  $ ./xmldump.exe <<EOF
  > <?xml version="1.0" ?>
  > <hello/>
  > EOF
  hello:

Can we do arbitraty texts?
  $ ./xmldump.exe <<EOF
  > <?xml version="1.0" ?>
  > <hello>this "test" should work</hello>
  > EOF
  hello:
    this "test" should work

Do we support CDATA?
  $ ./xmldump.exe <<EOF
  > <?xml version="1.0" ?>
  > <cdata><![CDATA[cdata works fine]]></cdata>
  > EOF
  cdata:
    cdata works fine

Do we support XML without preamble?
  $ echo '<yeah/>' | ./xmldump.exe
  yeah:

Do we support tabs?
  $ echo '<yeah>[\n\t\n\t\n]</yeah>' | ./xmldump.exe
  yeah:
    [ ]

Do we ignore spaces before real content?
  $ echo "<yeah><tab>\t|\t</tab><crlf>\r\n|\r\n</crlf></yeah>" | ./xmldump.exe
  yeah:
    tab:
      |
    crlf:
      |

Do we ignore processing instructions?
  $ echo "<yeah><peep><?this is a pi?></peep></yeah>" | ./xmldump.exe
  yeah:
    peep:
