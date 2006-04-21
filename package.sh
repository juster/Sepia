files=$(perl -e 'chomp(@x=<>);print join ",",@x' MANIFEST)
ver=$(perl -MSepia -e 'print $Sepia::VERSION')
cd ..
eval "tar czvf sepia-$ver.tgz sepia/{$files}"
