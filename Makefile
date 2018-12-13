Play: Brasilian.hs Cards.hs
	ghc -dynamic -o Play Brasilian.hs && rm -f *.o *.hi

clean:
	rm -f *.o *.hi Play
