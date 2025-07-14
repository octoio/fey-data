#!/opt/homebrew/bin/fish
# Simple test runner for fey-data

set_color green
echo "🧪 Running fey-data tests"
set_color normal

# Check if we're in the right directory
if not test -f "dune-project"
    set_color red
    echo "❌ Must be run from the fey-data directory"
    set_color normal
    exit 1
end

# Setup environment
eval (opam env --set-switch)

# Install dependencies if needed
if not opam list --installed | grep -q alcotest
    set_color blue
    echo "📦 Installing test dependencies..."
    set_color normal
    opam install alcotest --yes
end

# Build and test
set_color blue
echo "🔨 Building..."
set_color normal
dune build

if test $status -eq 0
    set_color blue
    echo "🧪 Running tests..."
    set_color normal
    dune runtest
    
    if test $status -eq 0
        set_color green
        echo "✅ All tests passed!"
        set_color normal
    else
        set_color red
        echo "❌ Some tests failed"
        set_color normal
        exit 1
    end
else
    set_color red
    echo "❌ Build failed"
    set_color normal
    exit 1
end

# Clean up if requested
if test "$argv[1]" = "--clean"
    set_color blue
    echo "🧹 Cleaning..."
    set_color normal
    dune clean
end
