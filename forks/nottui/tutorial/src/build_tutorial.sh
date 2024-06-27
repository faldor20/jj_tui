# Run the tangling

parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )

pushd "$parent_path" 
dune exec ../../tutorial/src/tangle.exe ./hackernews/bin ../hackernews
popd
