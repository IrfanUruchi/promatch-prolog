FROM swipl:stable

RUN apt-get update \
 && apt-get install -y --no-install-recommends curl \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY . /app

RUN mkdir -p /app/data

ENV PORT=3000

EXPOSE 3000

HEALTHCHECK --interval=10s --timeout=3s --retries=5 \
  CMD curl -fsS http://localhost:${PORT}/ >/dev/null || exit 1

CMD ["swipl", "-q", "-f", "main.pl"]
