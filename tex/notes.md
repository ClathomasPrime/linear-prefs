This might be enriching:
http://www.cse.unsw.edu.au/~haziz/comsoc10years.pdf

Combinatorics
====
    
[PS19] Condorcet Domains, Median Graphs and the Single-Crossing Property
https://arxiv.org/pdf/1507.08219.pdf

    Does lots of cool stuff. One of them: characterizes those sets for which a
    "representative voter theorem" holds. The answer: it's basically just single crossing,
    plus a very small list of other examples, all of them pretty similar to FlipFlop.
    
    This is a very, very cool paper. They put tonnes of combinatorial structure on top
    of preference domains, and their method is really effective. A core tool is median graphs.
    
[DK13] aximal Condorcet Domains. https://link.springer.com/article/10.1007/s11083-011-9235-z

    Very abstract / pure mathy but has nice (non complete) characterizations.

[DKK12] Condorcet domains of tiling type. Vladimir I. Danilova, Alexander V. Karzanovb, Gleb Koshevoya
https://www.sciencedirect.com/science/article/pii/S0166218X1100299X

    Concerns domains containing precicely one pair of reversed preferences, apparently.
    "Constructs large CDs via tiling" or something, too. 

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

[!] A characterization of the single-crossing domain. https://link.springer.com/article/10.1007/s00355-012-0717-8

    Inspired by [Ballester and Haeringer 2011], these guys give a "forbidden subconfiguration" characterization
    of single-crossing. Because their techniques are algorithmic, they also get a (fairly complicated)
    O(nm^2) time algorithm for checking single-crossing (and ordering voters accordingly).
    
    Forbidden subconfigs: 
      - gamma config: "none of the three voters can be between the other two"  (noRep and 3Cycle are good examples)
      - delta configs: "four voters, each pair of them must be adjacent" (basically FlipFlop!)

[!] Single-CrossingDifferencesonDistributions http://www.columbia.edu/~nk2339/Papers/KLR-sced.pdf

    Seems to hint at "two dimensional stuff is single crossing" but will take a lot of unpacking...

[!] Strategy-proofness and single-crossing, Alejandro Saporiti, 2009.
  https://econtheory.org/ojs/index.php/te/article/viewArticle/20090127 

    Nice and modern perspective. Probably valuable for a clear definition. 
    Proves e.g. that all strategyproof rules on a S.C. domain only take the top candidate of each voter
    into account. Peculiar... Actually I should really try harder to parse this.

Other domains
====
[Ballester and Haeringer 2011] 
A characterization of the single-peaked domain. https://link.springer.com/article/10.1007/s00355-010-0476-3

    Prove that single-peaded is equivalent to "worst-restricted" (among any tripple of outcomes, 
    some is never ranked worst) plus a "forbidden subconfiguration":
      - alpha config: voters with oposite opinions on 3 outcomes agree about a fourth
        (the preference set "4 > 1 > 2 > 3;    4 > 3 > 2 > 1" captures the whole idea)
    
    Mentions https://www.sciencedirect.com/science/article/pii/0167637786900726 had an algorithm 
    for checking SP as well.
    
    Also handle group separable, for which the preference set must be "medium restricted"
    (for all trippls, one is never ranked in the middel) forbidden subconfig:
      - beta config: the preference "1 > 2 > 3 > 4;    2 > 4 > 1 > 3"


Representative Voting
====
[!] Rothstein, P. (1991) Representative Voter Theorems.
Public Choice 72(2-3): 193-212

    Introduces a very cool notion of representative voter

See also [PS19]

Computational Complexity of checking a condition on a pref set
====

[!] Are There Any Nicely Structured Preference Profiles Nearby? 
citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.377.3567&rep=rep1&type=pdf

    Problem: how many {voters, outcomes} must you delete to get a _____ set of preferences

[!] Computational aspects of nearly single-peaked electorates. https://arxiv.org/abs/1211.2627

    Concurrent with previous paper, but focusing on single-peaked (with a bit more detail)

[!] On Recognising Nearly Single-Crossing Preferences http://www.dominik-peters.de/publications/nearlysc.pdf

    Continues this line of research further. Handles more edit distances from the "nearly single peaked" paper,
    applied to single crossing.

Cool thing for later: application to matching
====

[!] "The Uniqueness of Stable Matchings" http://pareto.uab.es/jmasso/pdf/ClarkCTE2006.pdf

    Defines "no crossing" for bipartite systems with preferences;
    studies how it relates to single crossing;
    shows it gives really good matchings.

Etc / Crazy Stuff
====

[!] Algebraic framework for voting theory.
https://www.math.hmc.edu/seniorthesis/archives/2005/zajj/zajj-2005-thesis.pdf

    Actually looking closer this is an undergrad thesis soooo
    
[!] Lirong Xia's book-like thesis: https://www.cs.rpi.edu/~xial/Files/dissertation_Lirong.pdf

    Might be good resource on manipulability. Mentions restricted domains.

[!] Sen et al. "Dictatorial Domains" https://link.springer.com/article/10.1007/s00199-002-0285-8

    Provides a sufficient condition for all strategyproof voting rules to be dictatorships.

[!] How similar are two elections? https://www.aaai.org/Papers/AAAI/2019/AAAI-FaliszewskiP2.3395.pdf

    (Surprisingly) efficient algorithms for measuring election similarity

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

    "strategic foundation of representative voter theorem":
    voting for a median representative then picking his favorite is SP.

[39] Saporiti, A. (forthcoming) On the existence of Nash equilibrium in
electoral competition. Game Theory and Information



Less useful links:
====
* https://link.springer.com/article/10.1007/BF02310791
* https://ac.els-cdn.com/S0169721810000183/1-s2.0-S0169721810000183-main.pdf?_tid=86eaab99-2cce-4db1-a3c8-8d4daf6c5149&acdnat=1548375032_fbbe4713536bfb8955ebb14d3abef1ac
* https://www.jstor.org/stable/pdf/30034333.pdf?refreqid=excelsior%3A6a1a5406c01df958074441a65909f29c
* https://pdfs.semanticscholar.org/9009/5f7ebf4466548aa51f979ac22ce0fa7f459f.pdf
* https://rangevoting.org/LauwersTopoSC.pdf
