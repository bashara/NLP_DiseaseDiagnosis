% FINAL PROJECT: Rithikha and Bashara

% Model queries:
/*question(['Ive', had, joint, pain, for, a, really, long, time], [], A).
question(['Ive', had, fever, for, a, really, long, time], [], A).
question(['I', have, had, itchy, spots, for, days], [], A).
question(['I', have, been, feeling, tired, for, a, really, long, time], [], A).
question(['My', knees, hurt, since, last, year], [], A).*/


% description_phrase(T0,T4,Diag) is true if
% T0 and T4 are list of words, such that T4 is an ending of T0
% and T0-T4 form a description phrase of the symptoms.
% Diag is the Disease that the description phrase is potentialy referring to
description_phrase(T0, T4, Diag, T1, T2, T3) :-
  det_phrase(T0, T1, Diag),
  symptom_phrase(T1, T2, Diag),
  preposition_phrase(T2, T3, Diag),
  duration_phrase(T3, T4, Diag).

% det_phrase(T0,T1,Diag) is true if T0-T1 is a det is true of Diag
det_phrase(T0,T2,Diag) :-
  det(T0,T1,Diag),
  det_phrase(T1,T2,Diag).
det_phrase(T,T,_).

% symptom_phrase(T0,T1,Diag) is true if T0-T1 is a symptom is true of Diag
symptom_phrase(T0,T2,Diag) :-
    symp(T0,T1,Diag),
    symptom_phrase(T1,T2,Diag).
symptom_phrase(T,T,_).

% preposition_phrase(T0,T1,Diag) is true if T0-T1 is a preposition/adjective is true of Diag
preposition_phrase(T0,T2, Diag) :-
   prep(T0,T1, Diag),
   preposition_phrase(T1,T2, Diag).
preposition_phrase(T,T,_).

 % duration_phrase(T0,T1,Diag) is true if T0-T1 is a duration is true of Diag
 duration_phrase(T0,T2,Diag) :-
     dur(T0,T1,Diag),
     duration_phrase(T1,T2,Diag).
 duration_phrase(T,T,_).

% DICTIONARY

det([had| T],T,_).
det([a| T],T,_).
det([have| T],T,_).
det([been| T],T,_).
det([having| T],T,_).
det([feeling| T],T,_).
det([feel| T],T,_).
det(['I'| T],T,_).
det(['My'| T],T,_).
det(['Ive'| T],T,_).

symp([H1 | T],T,Diag) :- symptom(H1, Diag).
symp([H1, H2 | T],T,Diag) :- symptom([H1, H2], Diag).
symp([H1, H2, H3 | T],T,Diag) :- symptom([H1, H2, H3], Diag).

prep([for | T],T,_).
prep([since | T],T,_).
prep([a| T],T,_).
prep([really| T],T,_).
prep([very| T],T,_).
prep([last| T],T,_).

dur([H1 | T],T,Diag) :- duration(H1, Diag).
dur([H1, H2 | T],T,Diag) :- duration([H1, H2], Diag).
dur([H1, H2, H3 | T],T,Diag) :- duration([H1, H2, H3], Diag).

% DATABASE

% symp chickenPox
symptom(fever, chickenPox).
symptom(tired, chickenPox).
symptom([loss, of, appetite], chickenPox).
symptom([appetite,loss], chickenPox).
symptom([not, hungry], chickenPox).
symptom([dont, want, to, eat], chickenPox).
symptom([itchy, spots], chickenPox).
symptom([itchy, bumps], chickenPox).
symptom([itchy, spots, everywhere], chickenPox).
symptom([bumps, everywhere], chickenPox).

% symp Cold
symptom(tired, cold).
symptom(sneezing, cold).
symptom([runny, nose], cold).
symptom(fever, cold).
symptom(coughing, cold).
symptom(cough, cold).
symptom([sore, throat], cold).

% symp arthritis
symptom([joint, pain], arthritis).
symptom([knees, hurt], arthritis).
symptom([wrists, hurt], arthritis).
symptom([fingers, hurt], arthritis).
symptom(stiff, arthritis).
symptom(swelling, arthritis).

% symp concussion
symptom(headache, concussion).
symptom(headaches, concussion).
symptom(dizziness, concussion).
symptom([blurred, vision], concussion).
symptom([trouble, concentrating], concussion).
symptom([concentration, issues], concussion).
symptom([poor, concentration], concussion).
symptom(depressed, concussion).

% symp shingles
symptom(fever, shingles).
symptom(tired, shingles).
symptom([loss, of, appetite], shingles).
symptom([appetite,loss], shingles).
symptom([not, hungry], shingles).
symptom([dont, want, to, eat], shingles).
symptom([itchy, spots], shingles).
symptom([itchy, bumps], shingles).
symptom([itchy, spots, everywhere], shingles).

% DURATION

duration([long,time], arthritis).
duration(month,arthritis).
duration(months,arthritis).
duration(year,arthritis).
duration(years,arthritis).

duration([couple, of, days], chickenPox).
duration([long,time], chickenPox).
duration(week,chickenPox).
duration(weeks,chickenPox).
duration(day,chickenPox).
duration(days,chickenPox).
duration([over, a, week], chickenPox).

duration(weeks, concussion).
duration(days, concussion).
duration(week, concussion).
duration(day, concussion).
duration(months, concussion).
duration(month, concussion).
duration(years, concussion).
duration(year, concussion).
duration([really, long, time], concussion).

duration(month, shingles).
duration([month,and, a, bit], shingles).
duration([over, a, month], shingles).
duration(months, shingles).

duration(week, cold).
duration([over, a, week], cold).
duration(days, cold).
duration(day, cold).


% DIAGNOSIS

diagnosis(S1,S2,S3,S4,chickenPox):-
  symp(S1,S2, chickenPox),
  dur(S3,S4,chickenPox).

diagnosis(S1,S2,S3,S4,arthritis):-
  symp(S1,S2, arthritis),
  dur(S3,S4, arthritis).

diagnosis(S1,S2,S3,S4,concussion):-
  symp(S1,S2, concussion),
  dur(S3,S4, concussion).

diagnosis(S1,S2,S3,S4,shingles):-
  symp(S1,S2, shingles),
  dur(S3,S4, shingles).

  diagnosis(S1,S2,S3,S4,cold):-
    symp(S1,S2, cold),
    dur(S3,S4, cold).

% USER INPUT AND OUTPUT

% question(Question,QR,Diagnosis) is true if Query provides an answer about Object to Question
question(T0,T1,Diag) :-
  description_phrase(T0,T1,Diag, S1, S2, S3), diagnosis(S1, S2, S3, T1, Diag).

% ask(Q,A) gives answer A to question Q
ask(Q,A) :-
  question(Q,[],A).

% To get the input from a line:
patient(Diagnosis) :-
write("Hi there, welcome to the online healthcare portal. Please tell us what symptom you are experiencing,
followed by how long you have had it for. We will do our best to figure out what's going on: "), flush_output(current_output),
  readln(Ln),
  question(Ln,End,Diagnosis),
  member(End,[[],['?'],['.']]).

% To add new symptoms and diagnoses:
new(Diagnosis) :-
  write("Looks like we can't find any matching diagnosis. Could you please write the symptom you are feeling again?"),
  readln([S|_]),
  write("and could you tell us how long you have been experiencing this symptom?"),
  readln([D|_]),
  write("We apologize that we couldn't help you, let us know what diagnosis you recieved for these symptoms and we will add it to our database for future reference"),
  readln([Diagnosis| _]),
  assert(symptom(S, Diagnosis)),
  assert(duration(D, Diagnosis)),
  assert((diagnosis(S1,S2,S3,S4, Diagnosis):-
    symp(S1,S2, Diagnosis),
    dur(S3,S4, Diagnosis))),
  write("Thank you, hope you feel better soon").
