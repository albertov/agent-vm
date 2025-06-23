#!/usr/bin/env python3

import json
import time
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.common.exceptions import WebDriverException

def test_chrome_config(description, options_dict):
    """Test a Chrome configuration and report results."""
    print(f"\n{'='*60}")
    print(f"Testing: {description}")
    print(f"Options: {json.dumps(options_dict, indent=2)}")
    print(f"{'='*60}")

    chrome_options = Options()

    # Add headless if specified
    if options_dict.get('headless', False):
        chrome_options.add_argument('--headless')

    # Add all arguments
    for arg in options_dict.get('arguments', []):
        chrome_options.add_argument(arg)

    try:
        print("Attempting to start Chrome...")
        driver = webdriver.Chrome(options=chrome_options)
        print("✅ Chrome started successfully!")

        # Try to navigate to a page
        print("Attempting to navigate to example.com...")
        driver.get("https://example.com")
        print("✅ Navigation successful!")

        # Get page title
        title = driver.title
        print(f"✅ Page title: {title}")

        driver.quit()
        print("✅ Chrome closed successfully!")
        return True

    except WebDriverException as e:
        print(f"❌ Failed: {str(e)[:200]}...")
        return False
    except Exception as e:
        print(f"❌ Unexpected error: {str(e)}")
        return False

# Test configurations
test_configs = [
    # Basic headless
    ("Basic headless", {
        "headless": True,
        "arguments": ["--no-sandbox", "--disable-setuid-sandbox"]
    }),

    # Minimal flags
    ("Minimal flags", {
        "headless": True,
        "arguments": ["--no-sandbox"]
    }),

    # All suggested flags
    ("All suggested flags", {
        "headless": True,
        "arguments": [
            "--no-sandbox",
            "--disable-setuid-sandbox",
            "--disable-dev-shm-usage",
            "--disable-gpu",
            "--disable-web-security",
            "--disable-features=VizDisplayCompositor",
            "--single-process",
            "--no-zygote"
        ]
    }),

    # Without single-process
    ("Without single-process", {
        "headless": True,
        "arguments": [
            "--no-sandbox",
            "--disable-setuid-sandbox",
            "--disable-dev-shm-usage",
            "--disable-gpu",
            "--disable-web-security",
            "--disable-features=VizDisplayCompositor",
            "--no-zygote"
        ]
    }),

    # Docker-style configuration
    ("Docker-style", {
        "headless": True,
        "arguments": [
            "--no-sandbox",
            "--disable-setuid-sandbox",
            "--disable-dev-shm-usage",
            "--disable-gpu",
            "--disable-software-rasterizer",
            "--disable-extensions",
            "--disable-default-apps",
            "--disable-translate",
            "--disable-sync",
            "--no-first-run",
            "--mute-audio",
            "--hide-scrollbars"
        ]
    })
]

# Summary
successful = []
failed = []

print("Starting Chrome headless tests...")
print(f"Testing {len(test_configs)} different configurations")

for description, config in test_configs:
    if test_chrome_config(description, config):
        successful.append(description)
    else:
        failed.append(description)
    time.sleep(2)  # Brief pause between tests

# Print summary
print("\n" + "="*60)
print("SUMMARY")
print("="*60)
print(f"Successful configurations: {len(successful)}")
for config in successful:
    print(f"  ✅ {config}")

print(f"\nFailed configurations: {len(failed)}")
for config in failed:
    print(f"  ❌ {config}")
