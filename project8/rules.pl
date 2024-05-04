pay(F,L,P) :- commission(F,L,Min,Sales,Crate),
            Sales*Crate > Min,
            P is Sales*Crate.
pay(F,L,P) :- commission(F,L,Min,Sales,Crate),
            Sales*Crate =< Min,
            P is Min.

pay(F,L,P) :- salaried(F,L,S),
            P is S.

pay(F,L,P) :- hourly(F,L,W,R),
            W =< 40,
            P is W*R.

pay(F,L,P) :- hourly(F,L,W,R),
            W > 40, W =< 50,
            P is 40*R + (W-40)*R*1.5.

pay(F,L,P) :- hourly(F,L,W,R),
            W > 50,
            P is 40*R + 10*R*1.5 + (W-50)*R*2.0.