Can we parse a nearly empty file?
  $ cat | ./xmldump.exe <<EOF
  > <?xml version="1.0" ?>
  > <hello></hello>
  > EOF
  hello:

Can we have space before root?
  $ cat | ./xmldump.exe <<EOF
  > <?xml version="1.0" ?>
  > 
  > <hello></hello>
  > EOF
  hello:

Can we parse a two-level deep node?
  $ cat | ./xmldump.exe <<EOF
  > <?xml version="1.0" ?>
  > <hello><world>test</world></hello>
  > EOF
  hello:
    world:
      test

Can we use exclamations?
  $ cat | ./xmldump.exe <<EOF
  > <?xml version="1.0" ?>
  > <hello>!!</hello>
  hello:
    !!

Can we mix spaces in text?
  $ cat | ./xmldump.exe <<EOF
  > <?xml version="1.0" ?>
  > <hello>i   am   groot   !   !</hello>
  hello:
    i am groot ! !

Can use use paths in text?
  $ cat | ./xmldump.exe <<EOF
  > <?xml version="1.0" ?>
  > <hello>../relative/path.txt</hello>
  hello:
    ../relative/path.txt

Can we ignore comments?
  $ cat | ./xmldump.exe <<EOF
  > <?xml version="1.0" ?>
  > <hello>[<!-- yeah -->]</hello>
  hello:
    [ ]

Can we ignore comments with dashes?
  $ cat | ./xmldump.exe <<EOF
  > <?xml version="1.0" ?>
  > <hello>[<!-- - yeah - -->]</hello>
  hello:
    [ ]

Can we process empty tags?
  $ cat | ./xmldump.exe <<EOF
  > <?xml version="1.0" ?>
  > <hello/>
  hello:

