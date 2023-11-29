#!/bin/bash
echo "#!/bin/sh
fourmolu -i ./game/src/Game/
FILES=\$(git diff --diff-filter=d --name-only)
git add \$FILES" > .git/hooks/pre-commit
chmod +x .git/hooks/pre-commit