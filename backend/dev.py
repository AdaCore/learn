from app import create_app

# Create the app
app = create_app()

if __name__ == "__main__":
    app.run(debug=True, host='0.0.0.0')