# How to deploy

    export LANG="en_US.UTF-8"
    export LC_ALL="en_US.UTF-8"
    git pull
    sleep 2
    rvm use 2.1.5
    bundle install
    rm -rf output/
    nanoc compile
    if [ ! "$?" -eq "0" ]; then
    	echo "Error compiling, not sending site..."
	    exit 1
    fi
    rsync --omit-dir-times -rltvz output/ webcorp.vpn.p1:/opt/www/docs.ejabberd.im/

# How to generate the docs site for local testing

    export LANG="en_US.UTF-8"
    export LC_ALL="en_US.UTF-8"
    git pull
    sleep 2
    rvm use 2.1.5
    bundle install
    rm -rf output/
    nanoc compile
    nanoc view

You can them point your browser to http://localhost:3000/
