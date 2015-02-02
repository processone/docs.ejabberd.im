# How to deploy

    export LANG="en_US.UTF-8"
    export LC_ALL="en_US.UTF-8"
    git pull
    sleep 2
    rvm use 1.9.3
    bundle install
    nanoc compile
    if [ ! "$?" -eq "0" ]; then
    	echo "Error compiling, not sending site..."
	    exit 1
    fi
    rsync --omit-dir-times -rltvz output/ mremond@webcorp.vpn.p1:/opt/www/developer.boxcar.io/
