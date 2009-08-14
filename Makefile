all: status

commit: update
	svn commit

update:
	svn update

status:
	svn status

diff:
	svn diff

.PHONY: commit update status diff
