# Configure git

# This line may be needed to change the default branch name from master to main.
git config --global init.defaultBranch main 

git config --global user.name ainnesgold
git config --global user.email ainnesgo@hawaii.edu

# Link your local repository to the origin repository on GitHub, by
# copying the code shown on your GitHub repo under the heading:
# "…or push an existing repository from the command line"


git remote add origin https://github.com/ainnesgold/handouts.git
git branch -M main
git push -u origin main
