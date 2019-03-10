This might be enriching:
http://www.cse.unsw.edu.au/~haziz/comsoc10years.pdf
  
Single Crossing
====
[!] Gans and Smart (1996). Majority voting with single-crossing preferences.
Journal of Public Economics. https://www.sciencedirect.com/science/article/pii/0047272795015035
    
    First paper which clarified single crossing and showed equivalence/generalization
    of many previous papers.
    They discusses ease of checking in practice (by this they 
    DONT mean computational ease, they mean that you can prove certain scenarios 
    end up being single crossing under certain assumptions, and you can easily prove things
    (I think - I really really don't understand all the economics mumbo jumbo)).

Representative Voting
====
[!] Rothstein, P. (1991) Representative Voter Theorems.
Public Choice 72(2-3): 193-212

    Introduces a very cool notion of representative voter

Computational Complexity of checking a condition on a pref set
====

[!] Are There Any Nicely Structured Preference Profiles Nearby? 
citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.377.3567&rep=rep1&type=pdf

    Problem: how many {voters, outcomes} must you delete to get a _____ set of preferences

[!] Computational aspects of nearly single-peaked electorates. https://arxiv.org/abs/1211.2627

    Concurrent with previous paper, but focusing on single-peaked (with a bit more detail)

[!] A characterization of the single-peaked domain. https://link.springer.com/article/10.1007/s00355-010-0476-3

    Prove that single-peaded is equivalent to "worst-restricted" (among any tripple of outcomes, 
    some is never ranked worst) plus a "forbidden subconfiguration" (they call it an alpha-configuration -
    the preference set is "4 > 1 > 2 > 3; 4 > 3 > 2 > 1" basically the whole idea).
    
    Mentions https://www.sciencedirect.com/science/article/pii/0167637786900726 had an algorithm 
    for checking SP as well.

[!] A characterization of the single-crossing domain. https://link.springer.com/article/10.1007/s00355-012-0717-8

    Inspired by the previous paper, these guys give a "forbidden subconfiguration" characterization
    of single-crossing. Because their techniques are algorithmic, they also get a (fairly complicated)
    O(nm^2) time algorithm for checking single-crossing (and ordering voters accordingly). 


How I first found single crossing:
=====
Excerpt from "Majority voting on restricted domains" introduction, page 2
http://eprints.lse.ac.uk/20114/1/Majority_voting_on_restricted_domains_(LSERO).pdf

The wealth of domain-restriction conditions for avoiding majority cycles was
supplemented by another family of conditions based not on ëleftí-ërightíorders
of the alternatives, but on ëleftí-ërightí orders of the individuals.  Important
conditions in this family are Grandmontís intermediateness [17] and Rothsteinís
order restriction ([36], [37]) with its special case of single-crossingness
(e.g., Roberts [34], Saporiti and TohmÈ [38], Saporiti [39]). To illustrate, a proÖle
of individual preferences is order-restricted if the individuals ñrather than the
alternatives ñcan be ordered from ëleftíto ërightísuch that, for each pair of
alternatives x and y , the individuals preferring x to y are either all to the left,
or all the right, of those preferring y to x

Aditional information/perspective from Wulf Gaertner's book

## intermediateness

[17] Grandmont, J.-M. (1978) Intermediate Preferences and the Majority Rule.
Econometrica 46(2): 317-330

    Preliminary work I think, not super important

## order restriction

[36] Rothstein, P. (1990) Order Restricted Preferences and Majority Rule.
So- cial Choice and Welfare 7(4): 331-342
    
    First real clear, core look at "order restricted" / "single crossing"

[37] Rothstein, P. (1991) Representative Voter Theorems.
Public Choice 72(2-3): 193-212

    Introduces a very cool notion of representative voter

## single-crossingness 

[34] Roberts, K. W. S. (1977) Voting over Income Tax Schedules.
Journal of Public Economics 8(3): 329-340

    "hierarchical adherence", a special case of order restricted (I think)

[38] Saporiti, A., TohmÈ, F. (2006) Single-crossing, strategic voting and the
median choice rule.  Social Choice and Welfare 26(2): 363-383

    "single crossing" is equivalent to order restricted

[39] Saporiti, A. (forthcoming) On the existence of Nash equilibrium in
electoral competition. Game Theory and Information



Less useful links:
====
* https://link.springer.com/article/10.1007/BF02310791
* https://ac.els-cdn.com/S0169721810000183/1-s2.0-S0169721810000183-main.pdf?_tid=86eaab99-2cce-4db1-a3c8-8d4daf6c5149&acdnat=1548375032_fbbe4713536bfb8955ebb14d3abef1ac
* https://www.jstor.org/stable/pdf/30034333.pdf?refreqid=excelsior%3A6a1a5406c01df958074441a65909f29c
* https://pdfs.semanticscholar.org/9009/5f7ebf4466548aa51f979ac22ce0fa7f459f.pdf
* https://rangevoting.org/LauwersTopoSC.pdf
