docker build --pull --rm -f "dockerfile" -t mbma:latest "." 
for i in {1..9}; do docker run -d -p 700$i:3838 --name mbma$i mbma; done
for i in {10..20}; do docker run -d -p 70$i:3838 --name mbma$i mbma; done
for i in {1..20}; do docker exec mbma$i mkdir /srv/shiny-server/$i; done
for i in {1..20}; do docker cp app.R mbma$i:/srv/shiny-server/$i; done 