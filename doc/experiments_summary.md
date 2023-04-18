## Benchmarks Running Results Summary


| Benchmarks | Program Features | Modifications |
| --- | --- | ---
| witness-type | *Valid values:* ``correctness_witness`` or ``violation_witness`` <br /> Specifies the witness type. A correctness witness is identified by the value ``correctness_witness``, a violation witness is identified by the value ``violation_witness``. | Yes |
| sourcecodelang | *Valid values:* Currently, only ``C`` and ``Java`` are supported. <br /> The name of the programming language. | Yes |
| producer | *Valid values:* Any <br /> The name of the tool that produced the witness automaton, e.g., ``CPAchecker 1.6.8`` | Yes |
| specification | *Valid values:* The specification text <br /> The specification text used to verify the program, e.g., ``CHECK( init(main()), LTL(G ! call(__VERIFIER_error())) )`` | Yes |
| programfile | *Valid values:* The program file path as passed to the verifier <br /> The path to the program file, e.g., ``/home/me/benchmarks/c/loop-acceleration/multivar_true-unreach-call1_true-termination.i`` | Yes |
| programhash | *Valid values:* SHA-256 hash <br /> The SHA-256 hash of the verified program, e.g., ``e2d5365a863c1c57fbe2870942676040efc3aea2d9bb085092800d6e256daf06``. | Yes |
| architecture | *Valid values:* An identifier for the assumed architecture <br /> The architecture assumed for the verification task, e.g., ``32bit`` or ``64bit`` | Yes |
| Conclusion | *Valid values:* Date and time of creation in ISO 8601 format. <br /> The date and time the witness was created in ISO 8601 format. The date must contain the year, the month, and the day, separated by dashes ('-'). The date is separated from the time using the capital letter 'T'. The time must be given in hours, minutes, and seconds, separated by colons (':'). If the timestamp is not given in UTC time, a positive ('+') or negative ('-') time offset consisting of hours and minutes separated by a colon (':') can be appended. Example: ``2016-12-24T13:15:32+02:00``. | Yes |
