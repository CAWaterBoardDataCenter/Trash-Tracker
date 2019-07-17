import http.server
import socketserver
import webbrowser


webbrowser.open('http://localhost:8080/', new=1)

PORT = 8080
Handler = http.server.SimpleHTTPRequestHandler
with socketserver.TCPServer(("", PORT), Handler) as httpd:
	print('serving at port: ', PORT)
	httpd.serve_forever()
