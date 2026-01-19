;; Bulletproof Update System
;; This file contains the rewritten update logic

;; Key insight: GitHub's browser_download_url redirects to a signed S3 URL
;; The signed URL does NOT accept Authorization headers - it will return 400
;; Solution: Don't pass auth header on redirects, or use API endpoint with Accept header

;; For private repos, we need to:
;; 1. Use the API endpoint: /repos/{owner}/{repo}/releases/assets/{asset_id}
;; 2. Set Accept: application/octet-stream header
;; 3. Include Authorization: token {token} header
;; 4. Follow redirects WITHOUT the auth header

;; Alternative approach that works:
;; 1. Get asset ID from release info
;; 2. Call API with Accept: application/octet-stream to get redirect
;; 3. Follow redirect without auth (signed URL doesn't need it)
