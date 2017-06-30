#!/usr/bin/env python

import http.server
from http.server import HTTPServer as SimpleHTTPServer
import socketserver
import logging

PORT = 8887

class GetHandler(http.server.SimpleHTTPRequestHandler):
    def do_GET(self):
        """Serve a GET request."""
        f = self.send_head()
        if f:
            try:
                logging.error(f)
                logging.error(f.__dict__)
                logging.error("START")
                self.copyfile(f, self.wfile)
                logging.error("DONE")
            finally:
                f.close()

Handler = GetHandler

if __name__ == "__main__":
    httpd = socketserver.TCPServer(("", PORT), Handler)
    print("Serving on port",PORT)
    httpd.serve_forever()
