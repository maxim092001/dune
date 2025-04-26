Dune should parse command stanza

  $ cat > dune-project << EOF
  > (lang dune 3.18)
  > EOF
  $ cat > dune << EOF
  > (command
  >  (name test-command)
  >  )
  > EOF
  $ dune build
