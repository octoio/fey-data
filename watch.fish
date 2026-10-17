#!/opt/homebrew/bin/fish
echo "Game Data Watcher Init"

# Read the JSON folder from the config file
set json_path (grep "^json_path" fey-data.config | cut -d'=' -f2 | string trim | string trim -c '"')
if test -z "$json_path"
    set json_path "../fey-rs/data/json"
end
echo "Using JSON folder: $json_path"

opam switch create . 5.2.0
opam switch
eval (opam env --set-switch)
opam update --yes && opam upgrade --yes
opam install . --deps-only
dune exec gamedata

# Function to run tests and handle errors
function run_tests_and_build
    echo (set_color blue)"🏗️  Building gamedata..."(set_color normal)
    if dune build
        echo (set_color blue)"🧪 Running tests..."(set_color normal)
        dune exec test/test_gamedata.exe
        set test_status $status
        if test $test_status -eq 0
            echo (set_color green)"✅ Tests passed!"(set_color normal)
            echo (set_color blue)"🔄 Running data pipeline"(set_color normal)
            dune exec gamedata
        else
            echo (set_color red)"❌ Tests failed!"(set_color normal)
            echo (set_color yellow)"Run './test.fish' for detailed test output."(set_color normal)
        end
    else
        echo (set_color red)"❌ Build failed!"(set_color normal)
    end
end

# Initial build and test
run_tests_and_build

while true
    echo "Watching files for changes..."
    # --no-defer    use more responsive interaction
    # -1            execute once
    # -E            use extended regular expressions
    # -i            include filter (overwrites any other filter)
    # -e            exclude filter (important, default behavior is to include all files not hit by any filter)
    # [folders]     folders to watch
    # debug pipe    | xargs -0 -n1 echo
    fswatch --no-defer -1 -E -i ".*/json/.*.json" -i "(bin|lib|test)/*.(ml|atd)" -e ".*(_build|Temp|Library|ProjectSettings|git).*" -e ".*(png|fish|py|entity-reference-indices.json|txt)" . "$json_path" - | xargs -0 -n1 echo
    
    echo (set_color cyan)"📁 File change detected, rebuilding..."(set_color normal)
    run_tests_and_build
end
