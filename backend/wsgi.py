from app import create_app

# Create the app
app = create_app(debug=True)

if __name__ == "__main__":
    app.run(host='0.0.0.0')