export PATH=../../bin:$PATH
clm -nt -IL Platform IPLookup -o IPLookup
clm -nt -IL Platform ProcessDemo -o ProcessDemo
clm -nt -IL Platform MapDemo -o MapDemo
clm -nt -IL Platform SQLDbDemo -o SQLDbDemo
clm -aC,-h,600m -nt -IL Platform -IL TCPIP WebDemo -o WebDemo
