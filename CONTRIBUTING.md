You are welcome to clone this repository and send us back pull requests.


## Coding guidelines

You should not:
- use tabs for indentation: use spaces instead (2 or 4 spaces are good enough);
- use more than 100 columns. Less than 80 is even better;
- leave trailing whitespace.

Otherwise, try to keep constitent with surrounding code and avoid super-weird construct
(unless it is really useful, very well documented and maintainable).


## Testing

Please verify that your modifications are valid:
- Add tests for your work in the `examples` directory.;
- Update the expected results found in the `check` directory according to your changes
and common sense (i.e. do not expect a result that is obviously wrong);
- Run `make check` and verify nothing is broken;
- Optionally (not regularly updated): compare your results to the one reported in the `results` directory
for better verification (some cases may not have their corresponding simplified test written yet).


## Misc

Any kind of contribution may be accepted, from fixing typos to adding a new killer feature
with its documentation.

If you would like to contribute but do not know where to start, you can still check the `TODO.md`

Seperating style-related, code cleaning and similar changes from functional ones would be nice.
Writing new tests (i.e. not something already existing) for existing features would be very nice.
Having fun would be the best.


Last but not least, **thank you for taking time to contribute!**
