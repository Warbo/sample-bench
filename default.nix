{ pkgs? import <nixpkgs> {} }:

with builtins;
with pkgs;
with rec {
  timeout   = callPackage ./timeout.nix {};

  usableNix =
    with {
      wrap = writeScript "wrap-nix" ''
        #!/usr/bin/env bash
        cat << EOF > "$2"
        #!/usr/bin/env bash
        [[ -n "\$NIX_PATH"   ]] || export   NIX_PATH="${getEnv "NIX_PATH"}"
        [[ -n "\$NIX_REMOTE" ]] || export NIX_REMOTE="${getEnv "NIX_REMOTE"}"
        exec '$1' "\$@"
        EOF
      '';
    };
    stdenv.mkDerivation {
      name = "usable-nix";
      buildCommand = ''
        mkdir -p "$out/bin"
        for F in "${nix}"/bin/*
        do
          NAME=$(basename "$F")
          "${wrap}" "$F" "$out/bin/$NAME"
          chmod +x "$out/bin/$NAME"
        done
      '';
    };

  env = buildEnv {
    name  = "sample-bench-env";
    paths = [ jq usableNix time timeout ];
  };

  fail = msg: ''{ echo -e "${msg}"; exit 1; }'';
};
stdenv.mkDerivation {
  name         = "sample-bench";
  src          = ./.;
  buildInputs  = [ env makeWrapper ];

  doCheck    = true;
  checkPhase = ''
    printf "Testing that benchmark needs arguments" 1>&2
    ./benchmark 2> /dev/null && ${fail "No args should fail"}

    echo "Testing benchmark of simple command" 1>&2
    RESULT=$(      CMD="echo"  \
             GEN_INPUT="echo"  \
                  INFO="x y z" \
                  REPS=3 ./benchmark) || ${fail "benchmark died"}
    echo "$RESULT" | jq '.' > /dev/null || ${fail "jq rejected our output"}

    echo "Testing that benchmark times out" 1>&2
    RESULT=$(GEN_INPUT="true"  \
                   CMD="${writeScript "tst" ''
                            #!/usr/bin/env bash
                            while true; do true; done
                          ''}" \
                  INFO=        \
                  REPS=2       \
              MAX_SECS=3 ./benchmark 2> stderr) ||
      ${fail "Couldn't bench with timeout\n$(cat stderr)"}
    ERR=$(cat stderr)

    L=$(echo "$RESULT" | jq '.results | length') ||
      ${fail "Couldn't get length"}
    [[ "$L" -eq 2 ]] || ${fail "Didn't get 2 reps:\n$RESULT\n$ERR"}

    F=$(echo "$RESULT" | jq '.results | map(.failure != null) | all') ||
      ${fail "Couldn't get failures"}
    [[ "x$F" = "xtrue" ]] || ${fail "Should have failed\n$ERR"}

    echo "Testing that benchmark memory usage is capped" 1>&2
    RESULT=$(GEN_INPUT="true"  \
                   CMD="${writeScript "tst" ''
                            #!/usr/bin/env bash
                            S="x"
                            while true; do X="$X$X"; done
                          ''}" \
                  INFO=        \
                  REPS=2       \
                MAX_KB=1000 ./benchmark 2> stderr) ||
      ${fail "Couldn't limit memory\n$(cat stderr)"}

    ERR=$(cat stderr)
    F=$(echo "$RESULT" | jq '.results | map(.failure != null) | all') ||
      ${fail "Couldn't get failures\n$ERR"}
    [[ "x$F" = "xtrue" ]] || ${fail "Should have failed\n$ERR"}
  '';

  installPhase = ''
    mkdir -p "$out/bin"
    cp benchmark "$out/bin"
    wrapProgram "$out/bin/benchmark" --prefix PATH : "${env}/bin"
  '';
}
