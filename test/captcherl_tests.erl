-module(captcherl_tests).

-export([turnstile_test/0, recaptcha_test/0, hcaptcha_test/0]).

-include_lib("stdlib/include/assert.hrl").


turnstile_test() ->
    % https://developers.cloudflare.com/turnstile/reference/testing/
    Data1 = {"1x0000000000000000000000000000000AA", "always pass"},
    Result1 = captcherl:verify(turnstile, Data1),
    ?assertEqual(true, Result1),

    Data2 = {"2x0000000000000000000000000000000AA", "should fail"},
    Result2 = captcherl:verify(turnstile, Data2),
    ?assertEqual(false, Result2).


recaptcha_test() ->
    % https://developers.google.com/recaptcha/docs/faq#id-like-to-run-automated-tests-with-recaptcha-v2-what-should-i-do
    Data1 = {"6LeIxAcTAAAAAGG-vFI1TnRWxMZNFuojJ4WifJWe", "always pass"},
    Result1 = captcherl:verify(recaptcha, Data1),
    ?assertEqual(true, Result1),

    Data2 = {"invalid secret", "should fail"},
    Result2 = captcherl:verify(recaptcha, Data2),
    ?assertEqual(false, Result2).


hcaptcha_test() ->
    % https://docs.hcaptcha.com/#integration-testing-test-keys
    Data1 = {"0x0000000000000000000000000000000000000000", "10000000-aaaa-bbbb-cccc-000000000001"},
    Result1 = captcherl:verify(hcaptcha, Data1),
    ?assertEqual(true, Result1),

    Data2 = {"0x0000000000000000000000000000000000000000", "should fail"},
    Result2 = captcherl:verify(hcaptcha, Data2),
    ?assertEqual(false, Result2).
