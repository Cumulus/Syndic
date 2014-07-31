echo "yes" | sudo add-apt-repository ppa:avsm/ppa
sudo apt-get update -qq
sudo apt-get install ocaml ocaml-native-compilers camlp4-extra opam opam

export OPAMYES=1
opam init
opam install oasis ocamlfind ocamlnet xmlm uri
eval `opam config env`

./configure
make

if [ "$TRAVIS_REPO_SLUG" == "Cumulus/Syndic" ] \
     && [ "$TRAVIS_PULL_REQUEST" == "false" ] \
     && [ "$TRAVIS_BRANCH" == "master" ]; then

  echo -e "Publishing ocamldoc...\n"

  git config --global user.email "romain.calascibetta@gmail.com"
  git config --global user.name "dinosaure"
  git clone --quiet --branch=gh-pages \
    https://${GH_TOKEN}@github.com/Cumulus/Syndic .documentation

  cd .documentation
  git fetch
  git merge master

  ./configure
  make doc

  git add -f doc/

  if [ -n "$(git status --untracked-files=no --porcelain)" ]; then
    git commit -m "Lastest ocamldoc on successful travis build $TRAVIS_BUILD_NUMBER auto-pushed to gh-pages"
    git push -fq origin gh-pages
  fi

  echo -e "Published ocamldoc to gh-pages.\n"
fi
