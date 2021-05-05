
offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,
period(2020-03-15, 2020-04-15), 10, 5), bus).
offerMean(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 1),
bus).
offerAccommodation(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,
period(2020-03-15, 2020-04-15), 10, 5), hotel).
offerAccommodation(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31),
10, 1), cabin).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), diving, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), snorkeling, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), horseRiding, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), snorkeling, 60).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), diving, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), horseRiding,
50).
customerPreferredMean(customer(ahmed, aly, 1993-01-30, single, 0, student), bus, 100).
customerPreferredMean(customer(mohamed, elkasad, 1999-01-30, single, 0, student), bus, 10).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), hotel, 20).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), cabin, 50).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), hotel,
100).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), cabin,
79).

         
            
			enough(L):-
                length(L,S),
                 S >= 2.				
             
			 
			 possibleSubset(L,L1) :-
			 possibleSubsetH(L,L2),
			 enough(L2),
			 per(L2,L1).
			 
			  possibleSubset(L,L1) :-
			 possibleSubsetH(L,L2),
			 not(enough(L2)),
			 L1 = L2.

              possibleSubsetH([],[]).

      possibleSubsetH([H|T1],[H|T]):-
	  possibleSubsetH(T1,T).

	  possibleSubsetH([_|T1],T):-
	  possibleSubsetH(T1,T).
	  
	 subset([], []).
subset([E|Tail], [E|NTail]):-
  subset(Tail, NTail).
subset([_|Tail], NTail):-
  subset(Tail, NTail).

 findActivity([],nil).
	   findActivity([H|T],A):-
		not(isActivity2(H)),
		findActivity(T,A).
		
	  findActivity([activity(A)|T],A).
				
	       replaceActivity(_,[],[]).
		    replaceActivity(N,[H|T],[H|T1]):-
		    not(isActivity2(H)),
		   replaceActivity(N,T,T1).
     replaceActivity(N,[activity(A)|T],[activity(N)|T]).

	      
	      
		 
		
			   
			   	  choosePreferences(Prefs, ChosenPreferences) :-
            findActivity(Prefs,A),
        possibleSubset(A,L),
		replaceActivity(L,Prefs,L1),
	     possibleSubsetH(L1,ChosenPreferences).

      insert2(X,[H|T],[X,H|T]).
	  insert2(X,[],[X]).

      insert(X,[H|T],[X,H|T]).
	  insert(X,[H|T],[H|T1]) :-
	        insert(X,T,T1).
	 
	  insert(X,[],[X]).
	  
	  perm([H|T],L) :-
	   findall([H|T],per([H|T],L1),L).
	   
	         per([],[]).

	  per([H|T],L):-
	   per(T,P),
	   insert(H,P,L).


	   
	
		   
		   
	  	getOffer([],O) :-
	   offerMean(O,_).

		
   
      
    getOffer([budget(B)|T],offer(A,C,N,D,E,F,G,H)) :-
	      	O = offer(A,C,N,D,E,F,G,H),
		   offerMean(O,_),
	        N =< B,
			getOffer(T,O).
	
	
	
	getOffer([accommodation(A)|T], Offer ) :-
	         offerAccommodation(Offer,A),
			 getOffer(T,Offer).
			
          % PERIOD			
		 getOffer([period(X,Y)|T],offer(A,B,C,D,F,E,G,H)) :-
	      	O = offer(A,B,C,D,F,E,G,H),
		   offerMean(O,_),
	     overlapPeriod(period(X,Y),E),
			getOffer(T,O).
			
			
	% MEANS
getOffer([means(M)|T4],O):-
	offerMean(O,M),
	getOffer(T4,O).
	
    % ACTIVITY

getOffer([activity(A)|T6],offer(De,Ac,B,VF,VT,P,DU,No)):-
	O = offer(De,Ac,B,VF,VT,P,DU,No),
	offerMean(O,_),
	isActivity(A,Ac),
	getOffer(T6,O).	
	
	%DESTINATION
getOffer([dest(De)|Ta],offer(De,B,Ac,VF,VT,P,DU,No)):-
	O=offer(De,B,Ac,VF,VT,P,DU,No),
	offerMean(O,_),
	getOffer(Ta,O).
	
	
	
	
	% ACTIVITY HELPER
isActivity([],L).
isActivity([H3|T5],L):-
	member(H3,L),
	isActivity(T5,L).
	
	% Activity 2 
	isActivity2(activity(_)).

getActivity(Customer,[],R,R).
getActivity(Customer,[H|T],R1,R) :-
   customerPreferredActivity(Customer,H,R2),
             R3 is R1 + R2,		
        getActivity(Customer,T,R3,R).
			 
			 
			 
			 

	    %%%%%%%%%%%%%%%%%%%% PREF SATISFACTION %%%%%%%%%%%%%%%%%%
		
		
		% Average Calculator 
		   average(L,Rate,S,S1) :-
	       L2 is L+1,
	    R is  div(Rate,L2),
	       S1 is S + R.
		   
		 isMean(means(_)).
		 isAccomodation(accommodation(_)).
		 
		 isNeutral([]).
		 isNeutral([H|T]):-
		 not(isMean(H)),
			not(isAccomodation(H)),
			not(isActivity2(H)),
			isNeutral(T).
		 
		   % preferenceSatisfaction(_,C,[H|T],S,0) :-
			%      isNeutral([H|T]),
			% 	  customerPreferredMean(C,_,_).
		   
		  preferenceSatisfaction(Offer,Customer,L,S) :-
		  preferenceSatisfactionH(Offer,Customer,L,0,S).
				 
	      
		
		
	         preferenceSatisfactionH(Offer,Customer,[H|T],S,S1):-
	        not(isMean(H)),
			not(isAccomodation(H)),
			not(isActivity2(H)),
	         preferenceSatisfactionH(Offer,Customer,T,S,S1). 
		
                          %%%%%ACCOMODATION%%%%%%%%%%%

     preferenceSatisfactionH(Offer, Customer, [accommodation(A)|T],S,S1):-
	             offerAccommodation(Offer,A),
				 customerPreferredAccommodation(Customer,A,R),
	                  S3 is S + R,
		         preferenceSatisfactionH(Offer, Customer, T,S3,S1).
				 
				   preferenceSatisfactionH(Offer, Customer, [accommodation(A)|T],S,S1):-
	              \+( offerAccommodation(Offer,A)),
		         preferenceSatisfactionH(Offer, Customer, T,S,S1).
				 
				              %%%%%%%%%%%%%%%%%%%%
		
                  %%%%%%%%%%%%% ACTIVITY %%%%%%%%%%%%%%
				  
	   preferenceSatisfactionH(offer(_,Ac,_,_,_,_,_,_), Customer, [activity(A)|T],S,S1):-
	                   getActivity(Customer,A,0,R),
	                  S3 is S + R,
		         preferenceSatisfactionH(offer(_,Ac,_,_,_,_,_,_), Customer, T,S3,S1).
				 
		preferenceSatisfactionH(offer(_,Ac,_,_,_,_,_,_), Customer, [activity(A)|T],S,S1):-
	              \+(isActivity(A,Ac)),
		         preferenceSatisfactionH(offer(_,Ac,_,_,_,_,_,_), Customer, T,S,S1).
				 
				 
				 
			%%%%%%%%%%%% MEANS OF TRANSPORT %%%%%%%%%%%%%%%%%%

preferenceSatisfactionH(Offer,Customer,[means(M)|T],S,S1):-
		offerMean(Offer,M),
		customerPreferredMean(Customer,M,R),
		S3 is S+R,
		preferenceSatisfactionH(Offer,Customer,T,S3,S1).


  preferenceSatisfactionH(Offer,Customer,[means(M)|T],S,S1):-
		not(offerMean(Offer,M)),
		preferenceSatisfactionH(Offer,Customer,T,S,S).



                   %%%%%%%% BASE CASE %%%%%%%%%% 
 
								   
		
      preferenceSatisfactionH(Offer, Customer, [] ,S1,S1) :-
	        customerPreferredMean(Customer,_,_).
       



                         %%%%%%%%%%%%%%%%		
						 
	  
	   %%%%%%%%%%%%%%%%%% recommendOfferForCustomer %%%%%%%%%%%%%%%%%
	   
	   recommendOfferForCustomer(Prefs,ChosenPrefs,Offer) :-
	              choosePreferences(Prefs,ChosenPrefs),
				  getOffer(ChosenPrefs,Offer).
				  
		    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
				  
				  findOffer([],[],O).

				  findOffer([H|T],[C|T1],O):-
				  recommendOfferForCustomer(H,C,O),
				  findOffer(T,T1,O).
				  
				
				  
				  
				  
				  findSatisfaction(_, [] ,_,[]).
        


	  %% findSatisfaction(Offer,[Customers],[ChosenPrefs],S) %%
				  findSatisfaction(H,[P|V],[R|B],[S0|S1]):-
				  preferenceSatisfaction(H,P,R,S0),
				  
				  findSatisfaction(H,V,B,S1).
				  
		
		%% chooseSatisfaction( Ratings , Offer , HighestN)	
		
		chooseSatisfaction([],_,L).
		chooseSatisfaction(S,H,[L1,L]):-
		       choose(S,H,0,L1),
			   chooseSatisfaction(S,H,L).
			   
			  
			   
		  % findChosen(Offer , [ Customers ] , [ S ] , Chosenprefs of these satisfaction ratings ).
		  findChosen(Offer,[H|T] , [S0|S1] , [C0|C1] ) :-
		           preferenceSatisfaction(Offer,H,C0,0,S0),
				   findChosen(Offer,T,S1,C1).
		  
             
           %%%%%%%%%%%%%%  recommendOffer  %%%%%%%%%%%%%%%%%%%	
	recommendOffer(Customers, Prefs, Offer, CustomersChosen):-
            findOffer(Prefs,C,Offer), 
			findSatisfaction(Offer,Customers,C,S),
			choose(S,C,Offer,0,Highest,Co),
			findSatisfaction(Offer,CustomersChosen,Co,Highest).
			
	
	
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
	
getAllActivities(L):-setof(X,Y^Z^customerPreferredActivity(Y,X,Z),L).
	

mostPreferredActivityH(C,[H|T],M,A) :-
            customerPreferredActivity(C,H,S),
			S =< M,
			mostPreferredActivityH(C,T,M,A).

mostPreferredActivityH(C,[H|T],M,A) :-
            customerPreferredActivity(C,H,S),
			S > M,
			mostPreferredActivityH(C,T,S,A).	 
			
mostPreferredActivityH(C,[],S,A):-
          customerPreferredActivity(C,A,S).

mostPreferredActivity(C,A) :-
             getAllActivities(L),
             mostPreferredActivityH(C,L,0,A).
			
			
remove(X,[X|List1],List1).
remove(X,[Y|List],[Y|List1]):-
X \= Y,
remove(X,List,List1).

	



	%choose([],nil,_,[]).

    choose([H|T],[C0|C1],offer(_,_,_,_,_,_,_,N),Co,[F|L],[C|Ct]):-
	Co < N,
	max([H|T],F),
	findPos(F,[H|T],Pos),
	findPos(C,[C0|C1],Pos),
	remove(F,[H|T],No),
	remove(C,[C0|C1],Po),
	CF is Co+1,
	choose(No, Po ,offer(_,_,_,_,_,_,_,N),CF,L,Ct).
	
	choose([H|T],[],offer(_,_,_,_,_,_,_,N),Co,[F|L],[]) :-
	Co < N,
	max([H|T],F),
	remove(F,[H|T],No),
    CF is Co+1,
	choose(No, [] ,offer(_,_,_,_,_,_,_,N),CF,L,[]).

	
	
			choose([H|T],[S|D],offer(_,_,_,_,_,_,_,N),N,[],[]).
			
			%choose([H|T],[],offer(_,_,_,_,_,_,_,N),N,[],[]).

			%choose([_],[],offer(_,_,_,_,_,_,_,N),N,[],[]).


			choose([],[],offer(_,_,_,_,_,_,_,_),_,[],[]).

	

    choose2( [] , _ , _, []).
    choose2( [H|T] , N ,N, []).


	choose2( [H|T] ,Co ,N ,[B|K] ) :-
    Co < N , 
    max([H|T],B),
	remove(B,[H|T],No),
	Cf is Co+1,
	choose2( No, Cf , N,K).

	


	
	
% find(Element,List,Position)
findPos(X,[X|T],0).
findPos(X,[H|T],P) :-
      findPos(X,T,P1),
	  P is P1 + 1.
       


	

max([X],X).
max([X,Y|Rest],Max):-
	max([Y|Rest],MaxRest),
	max(X,MaxRest,Max).	
max(X,Y,X):-
X>=Y.
max(X,Y,Y):-
	Y>=X.







		 sum_list([], ResultSoFar, FinalResult):-
FinalResult=ResultSoFar.
sum_list([H|T], ResultSoFar, FinalResult):-
NewAcc is ResultSoFar + H,
sum_list(T,NewAcc,FinalResult).











isf(R):-
   R @< (2000-03-3).
   
 overlapPeriod(period(X,Y),period(X,Y)).
   
 overlapPeriod(period(X,Y),period(N,K)) :-
        X @> (N),
		K @>= (X).
		
overlapPeriod(period(X,Y),period(N,K)) :-
        X @< (N),
		Y @>= (N).
		
overlapPeriod2(period(X,Y)) :-
   X @< (2000-03-2),
   Y @> (2020-01-1).




parseDate(R,date(year(X),month(Y),day(Z))):-
    split_string(R,"-"," ",[H0,T1,T2]),
	atom_number(H0,H1),
	atom_number(T1,T3),
	atom_number(T2,T4),
	X=H1,
	Y=T3,
	Z=T4.
	
	


append1([],L,L).
append1([H|T],L,[H|T1]):- append1(T,L,T1).



