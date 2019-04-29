import GIA
import Data.Set as S
import GG2GIA

--show the GG

gg= Bra (S.fromList [Seq [Act ("A", "C") "m", Act ("B", "C") "x"], Seq [Act ("B", "C") "n", Act ("A", "C") "y"]]) 

gg1= Seq [Act ("A", "B") "m", Act ("C", "B") "n"] 

gg2= Bra (S.fromList [Act ("A", "B") "m", Act ("A", "C") "n"]) 

gg3= Bra (S.fromList [Act ("A", "B") "m", Act ("A", "B") "n"]) 

gg4= Bra (S.fromList [Seq [Act ("A", "B") "m", Act ("B", "C") "x"], Seq [Act ("A", "C") "n", Act ("C", "B") "y"]]) 

a0=(S.fromList ["0", "1"], "0", S.fromList ["A"], S.fromList [(("A","C"), Send, "m"), (("A","C"), Send, "y")], S.fromList [("0", Interf (("A","C"), Send, "m"), "1"), ("0", Interf (("A","C"), Send, "y"), "1")])


b0=(S.fromList ["0", "1","2", "3"], "0", S.fromList ["C"], S.fromList [(("A","C"), Receive, "m"), (("A","C"), Receive, "y"), (("B","C"), Receive, "x"), (("B","C"), Receive, "m")], S.fromList [("0", Interf (("A","C"), Receive, "m"), "1"), ("0", Interf (("B","C"), Receive, "n"), "2"), ("1", Interf (("B","C"), Receive, "x"), "3"), ("2", Interf (("A","C"), Receive, "y"), "3")])

--show the product1 of g'
a=(S.fromList ["0", "1", "2", "3"], "0", S.fromList ["A"], S.fromList [(("A","B"), Send, "m"), (("A","C"), Send, "n")], S.fromList [("0", Interf (("A","B"), Send, "m"), "1"), ("0", Interf (("A","C"), Send, "n"), "2"), ("1", Tau, "3"),("2", Tau, "3")])

b=(S.fromList ["0", "1","2", "3"], "0", S.fromList ["B"], S.fromList [(("B","C"), Send, "x"), (("A","B"), Receive, "m"), (("C","B"), Receive, "y")], S.fromList [("0", Interf (("A","B"), Receive, "m"), "1"), ("1", Interf (("B","C"), Send, "x"), "3"), ("2", Interf (("C","B"), Receive, "y"), "3"), ("0", Tau, "2")])

c=(S.fromList ["0", "1", "2", "3"], "0", S.fromList ["C"], S.fromList [(("B","C"), Receive, "x"), (("A","C"), Receive, "n"), (("C","B"), Send, "y")], S.fromList [("0", Interf (("A","C"), Receive, "n"), "2"), ("2", Interf (("C","B"), Send, "y"), "3"), ("1", Interf (("B","C"), Receive, "x"), "3"), ("0", Tau, "1")])


--show the product1
a1=(S.fromList ["0", "1"], "0", S.fromList ["A"], S.fromList [(("A","B"), Send, "m")], S.fromList [("0", Interf (("A","B"), Send, "m"), "1")])

b1=(S.fromList ["0", "1"], "0", S.fromList ["B"], S.fromList [(("A","B"), Receive, "m")], S.fromList [("0", Interf (("A","B"), Receive, "m"), "1")])

--show the product
a2=(S.fromList ["0", "1"], "0", S.fromList ["A"], S.fromList [(("A","C"), Send, "m")], S.fromList [("0", Interf (("A","C"), Send, "m"), "1")])

b2=(S.fromList ["0", "1"], "0", S.fromList ["B"], S.fromList [(("B","C"), Send, "n")], S.fromList [("0", Interf (("B","C"), Send, "n"), "1")])

c2=(S.fromList ["0", "1", "2"], "0", S.fromList ["C"], S.fromList [(("A","C"), Receive, "m"), (("B","C"), Receive, "n")], S.fromList [("0", Interf (("A","C"), Receive, "m"), "1"), ("1", Interf (("B","C"), Receive, "n"), "2")])

--show the error state

a3=(S.fromList ["0", "1", "2"], "0", S.fromList ["A"], S.fromList [(("A","B"), Send, "m"), (("A","B"), Send, "n")], S.fromList [("0", Interf (("A","B"), Send, "m"), "1"), ("1", Interf (("A","B"), Send, "n"), "2")])

b3=(S.fromList ["0", "1"], "0", S.fromList ["B"], S.fromList [(("A","B"), Receive, "n"), (("A","B"), Receive, "m")], S.fromList [("0", Interf (("A","B"), Receive, "n"), "1")])

b3'=(S.fromList ["0", "1", "2"], "0", S.fromList ["B"], S.fromList [(("A","B"), Receive, "m"), (("A","B"), Receive, "n")], S.fromList [("0", Interf (("A","B"), Receive, "n"), "1"), ("1", Interf (("A","B"), Receive, "m"), "2")])

ab3=(S.fromList ["0", "1", "2", "3"], "0", S.fromList ["A", "B"], S.fromList [(("A","C"), Send, "m"), (("B","C"), Send, "y"), (("A","C"), Send, "x") ], S.fromList [("0", Interf (("A","C"), Send, "m"), "1"), ("1", Interf (("B","C"), Send, "y"), "2"), ("2", Interf (("A","C"), Send, "m"), "3"), ("2", Interf (("A","C"), Send, "x"), "3")])

c3=(S.fromList ["0", "1", "2", "3"], "0", S.fromList ["C"], S.fromList [(("A","C"), Receive, "m"), (("B","C"), Receive, "y"), (("A","C"), Receive, "x") ], S.fromList [("0", Interf (("A","C"), Receive, "m"), "1"), ("1", Interf (("A","C"), Receive, "x"), "2"), ("2", Interf (("B","C"), Receive, "y"), "3")])

--show the remove tau
t1=S.fromList [("0", Tau, "1"), ("1", Interf (("A","B"), Send, "m"), "3"), ("0", Tau, "2"), ("2", Interf (("A","B"), Send, "m"), "3")]

x=(S.fromList ["0", "1", "2", "3"], "0", S.fromList ["A"], S.fromList [(("A","B"), Send, "m")], S.fromList [("0", Tau, "1"), ("1", Interf (("A","B"), Send, "m"), "3"), ("0", Tau, "2"), ("2", Interf (("A","B"), Send, "m"), "3")])

t2=S.fromList [("0", Tau, "1"), ("1", Interf (("A","C"), Send, "n"), "3"), ("2", Tau, "3"), ("0", Interf (("A","B"), Send, "m"), "2")]

y=(S.fromList ["0", "1", "2", "3"], "0", S.fromList ["A"], S.fromList [(("A","B"), Send, "m"), (("A","C"), Send, "n")], S.fromList [("0", Tau, "1"), ("1", Interf (("A","C"), Send, "n"), "3"), ("2", Tau, "3"), ("0", Interf (("A","B"), Send, "m"), "2")])

t3=S.fromList [("0", Tau, "1"), ("1", Interf (("C","A"), Receive, "n"), "3"), ("2", Tau, "3"), ("0", Interf (("A","B"), Send, "m"), "2")]

z=(S.fromList ["0", "1", "2", "3"], "0", S.fromList ["A"], S.fromList [(("C","A"), Receive, "n"), (("A","B"), Send, "m")], S.fromList [("0", Tau, "1"), ("1", Interf (("C","A"), Receive, "n"), "3"), ("2", Tau, "3"), ("0", Interf (("A","B"), Send, "m"), "2")])

t4=S.fromList [("0", Tau, "1"), ("0", Interf (("A","B"), Send, "m"), "1")]

w=(S.fromList ["0", "1"], "0", S.fromList ["A"], S.fromList [(("A","B"), Send, "m")], S.fromList [("0", Tau, "1"), ("0", Interf (("A","B"), Send, "m"), "1")])


--show unwf GG1
a4=(S.fromList ["0", "1"], "0", S.fromList ["A"], S.fromList [(("A","B"), Send, "m"), (("A","C"), Send, "n")], S.fromList [("0", Interf (("A","B"), Send, "m"), "1"), ("0", Interf (("A","C"), Send, "n"), "1")])

b4=(S.fromList ["0", "1"], "0", S.fromList ["B"], S.fromList [(("A","B"), Receive, "m")], S.fromList [("0", Interf (("A","B"), Receive, "m"), "1"), ("0", Tau, "1")])

c4=(S.fromList ["0", "1"], "0", S.fromList ["C"], S.fromList [(("A","C"), Receive, "n")], S.fromList [("0", Interf (("A","C"), Receive, "n"), "1"), ("0", Tau, "1")])

--show unwf GG2
a5=(S.fromList ["0", "1"], "0", S.fromList ["A"], S.fromList [(("A","B"), Send, "m"), (("A","B"), Send, "n")], S.fromList [("0", Interf (("A","B"), Send, "m"), "1"), ("0", Interf (("A","B"), Send, "n"), "1")])

b5=(S.fromList ["0", "1"], "0", S.fromList ["B"], S.fromList [(("A","B"), Receive, "m"), (("A","B"), Receive, "n")], S.fromList [("0", Interf (("A","B"), Receive, "m"), "1"), ("0", Interf (("A","B"), Receive, "n"), "1")])
