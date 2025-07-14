#!/opt/homebrew/bin/fish
echo "Game Data Watcher Init"

# Read game root from config file
set game_root (grep "^game_root" fey-data.config | cut -d'=' -f2 | string trim | string trim -c '"')
if test -z "$game_root"
    set game_root "../fey-game-mock"
end
echo "Using game root: $game_root"

opam switch create . 5.2.0
opam switch
eval (opam env --set-switch)
opam update --yes && opam upgrade --yes
opam install . --deps-only
dune exec gamedata

# Function to run tests and handle errors
function run_tests_and_build
    echo (set_color blue)"🧪 Running tests..."(set_color normal)
    
    # Run tests first
    if dune exec test/test_gamedata.exe >/dev/null 2>&1
        echo (set_color green)"✅ Tests passed!"(set_color normal)
        echo (set_color blue)"🏗️  Building gamedata..."(set_color normal)
        dune exec gamedata
    else
        echo (set_color red)"❌ Tests failed! Skipping build."(set_color normal)
        echo (set_color yellow)"Run './test.fish' for detailed test output."(set_color normal)
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
    fswatch --no-defer -1 -E -i ".*/json/.*.json" -i "(bin|lib|test)/*.(ml|atd)" -e ".*(_build|Temp|Library|ProjectSettings|git).*" -e ".*(cs|meta|png|fish|py|uss|entity-reference-indices.json|txt)" . "$game_root/Assets/StreamingAssets" - | xargs -0 -n1 echo
    
    echo (set_color cyan)"📁 File change detected, rebuilding..."(set_color normal)
    run_tests_and_build
end
