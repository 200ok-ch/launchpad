FROM nginx

ENV BASIC_AUTH_USER=launchpad
ENV BASIC_AUTH_PASSWORD=launchpad

ADD public/* /usr/share/nginx/www/
ADD htaccess /usr/share/nginx/www/
RUN htpasswd -cb /usr/share/nginx/htpasswd ${BASIC_AUTH_USER} ${BASIC_AUTH_PASSWORD}
