{ pkgs? import <nixpkgs> {} }:

with builtins;
with pkgs;
with rec {
  timeout = stdenv.mkDerivation rec {
    name = "timeout";
    src  = fetchFromGitHub {
      owner  = "pshved";
      repo   = "timeout";
      rev    = "1ce1006";
      sha256 = "0nsv05kg22l6w0v885nli2hc7r6vi0jrfhb98jyfq38qaad5y78c";
    };

    # Wrapper for timeout, which provides sensible defaults
    withTimeout = writeScript "with-timeout" ''
      #!/usr/bin/env bash
      [[ -n "$MAX_SECS" ]] || export MAX_SECS=3600
      [[ -n "$MAX_KB"   ]] || export MAX_KB=2000000
      timeout -c --no-info-on-success -t "$MAX_SECS" -m "$MAX_KB" "$@"
    '';

    buildInputs  = [ makeWrapper ];
    patchPhase   = ''
      sed -e 's@/usr/bin/perl@${perl}/bin/perl@g' -i timeout
    '';
    installPhase = ''
      mkdir -p "$out/bin"
      cp timeout "$out/bin"
      cp "$withTimeout" "$out/bin/withTimeout"
      wrapProgram "$out/bin/timeout"     --prefix PATH : "${perl}/bin"   \
                                         --prefix PATH : "${procps}/bin" \
                                         --prefix PATH : "${coreutils}/bin"
      wrapProgram "$out/bin/withTimeout" --prefix PATH : "$out/bin"
    '';
  };

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

  fail = msg: "{ echo -e '${msg}'; exit 1; }";
};
stdenv.mkDerivation {
  name         = "sample-bench";
  src          = ./.;
  buildInputs  = [ env makeWrapper ];

  doCheck    = true;
  checkPhase = ''
    ./benchmark 2> /dev/null && ${fail "No args should fail"}
    RESULT=$(CMD="echo" GEN_INPUT="echo" INFO="x y z" REPS=3 ./benchmark) ||
      ${fail "benchmark died"}
    echo "$RESULT" | jq '.' > /dev/null || ${fail "jq rejected our output"}
  '';

  installPhase = ''
    mkdir -p "$out/bin"
    cp benchmark "$out/bin"
    wrapProgram "$out/bin/benchmark" --prefix PATH : "${env}/bin"
  '';
}
