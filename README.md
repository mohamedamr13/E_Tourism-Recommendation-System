# E_Tourism-Recommendation-System
A System built using SWI Prolog that uses pattern matching to match the best possible vacation offer for the user based on his or her preferences in the database.

#### Prolog is a logical programming language relying on First Order Logic to match patterns and answer queries and is used in AI and Linguistics. You can learn more about it [here](https://www.geeksforgeeks.org/prolog-an-introduction/)

#### The system simulates the knowledge database of a travel agency that includes the prefernces of the customers _ accomodation , destination , pricing , et cetera. The system tries then to satisfy as much of those preferences as much as possible and recommend an offer accordingly. 

#### The system could also recommend an offer to a group of people, as each offer could be scheduled to amaximum of N guests. Each customer will provide his list of preferences and the system will try to choose a set of preferences that is satisfied by an offer. It will then pick maximum N customers who will be most satisfied by the offer


## Database Structure

The code below shows how information about the agency's existing offers is fed into the system. Every fact includes structures like offers, periods , and lists that will be used in the pattern matching process 

```prolog
offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,
period(2020-03-15, 2020-04-15), 10, 5), bus).
offerMean(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 1),
bus).
offerAccommodation(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,
period(2020-03-15, 2020-04-15), 10, 5), hotel).
offerAccommodation(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31),
10, 1), cabin).

```


Likewise customer preferences are given in the form of facts with structures like *customer*

```prolog
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

```

## Get Offer
The predicate getOffer(ChosenPrefs,Offer) gets an offer Offer to match the list of preferences ChosenPrefs. The prefernces can include budget , accomomodation , period , means of transport , activities , and destination


```prolog
    % Empty List
  	getOffer([],O) :-
	   offerMean(O,_).

		% Budget
 getOffer([budget(B)|T],offer(A,C,N,D,E,F,G,H)) :-
	      	O = offer(A,C,N,D,E,F,G,H),
		      offerMean(O,_),
	        N =< B,
			    getOffer(T,O).
	
	
	% Accommodation
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
```

