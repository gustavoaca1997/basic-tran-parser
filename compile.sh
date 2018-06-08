alex Lex.x
happy Parser.y

case $(whoami) in
    german)
        stack ghc -- Lex.hs Parser.hs Main.hs -o BasicTran
        ;;
    *)
        ghc Main.hs -o BasicTran
        ;;
esac
