# CPAchecker: SV-COMP 2019 version
- Download: https://gitlab.com/sosy-lab/sv-comp/archives-2019/raw/svcomp19/2019/cpa-seq.zip
- How to run:    
    ```
    scripts/cpa.sh -svcomp19 -heap 10000M -timelimit 900s -preprocess
    ```

- Note: 
    + Use the exact name `__VERIFIER_error`
  
# CPAchecker: latest version
- Repo: https://github.com/sosy-lab/cpachecker
- Build: `ant` (or `ant jar` for creating jar)
- How to run:  
    ```
    scripts/cpa.sh -config config/predicateAnalysis.properties -spec /cpathriftserver/src/main/resource/sv-comp-reachability.spc -preprocess
    ```

