files=$(perl -e 'chomp(@x=<>);print join ",",@x' MANIFEST)
cd ..
eval "tar czvf sepia-$1.tgz sepia/{$files}"
