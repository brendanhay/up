// Attempted cross browser compatible JavaScript for submitting a form, minor visual cues,
// and a XMLHttpRequest to poll the server for progress updates.
(function() {
    
    var pageLoad = function() {    
        // Global timer for progress poll.
        var timer;
        var clearTimer = function() {
            if (undefined !== timer) { 
                window.clearInterval(timer); 
            }
        };
    
        // First time I've ever knowingly used `eval`, honest.
        var noDependenciesNoProblemCrossBrowserJsonParse = function(response) {
            return eval(['(', response, ')'].join(''));
        }
                        
        var trackProgress = function() {
            var xhr = window.XMLHttpRequest ? new XMLHttpRequest() : new ActiveXObject("Microsoft.XMLHTTP");
            var url = ['/progress/', document.getElementById('key').value, '?_method=get'].join(''); 
            xhr.open('GET', url, true);
            xhr.setRequestHeader('Content-Type', 'application/javascript'); 
            xhr.setRequestHeader('Cache-Control', 'no-cache'); 
            xhr.onerror = function() {
                hideProgress(false);
            };
            xhr.onreadystatechange = function() {
                if (xhr.readyState == 4) {
                    if (xhr.status == 200) { 
                        var response = noDependenciesNoProblemCrossBrowserJsonParse(xhr.responseText);
                        updateProgress(response); 
                    } else {
                        hideProgress(false);
                    }
                }
            };                           
            xhr.send(null);  
              
            document.getElementById('file').disabled = true;
            document.getElementById('description').disabled = true;                
        }
               
        // Remove any existing timer and set a new one to poll for progress.           
        var startProgress = function() {
            clearTimer();
            timer = window.setInterval(function() { trackProgress(); }, 250);            
        }    
        
        // Update the progress bar's width by setting it to the response.percent,
        // or hide the progress on error or completed.
        var progressBar = document.getElementById('progress');
        var updateProgress = function(response) {  
            console.log(response);
            var percent = response.percent;          
            progressBar.style.width = percent + '%';  
            if (!(response.id > 0)) {
                hideProgress(false);
            } else if (percent > 99) { 
                hideProgress(response);
            }
        }
        
        // Hairy ass function to get the `filename` from the `file` input, accounting for Windows paths.
        var getFileName = function() {
            var path = document.getElementById('file').value;
            var index = (path.indexOf('\\') >= 0 ? path.lastIndexOf('\\') : path.lastIndexOf('/'));
            var name = path.substring(index);
            if (name.indexOf('\\') === 0 || name.indexOf('/') === 0) {
                name = name.substring(1);
            }        
            return name;
        }; 
                
        // Hide the progress and show either an error or success div based on the response.        
        var uploadStatus = document.getElementById('upload_status');
        var hideProgress = function(response) { 
            clearTimer();
            if (!response) {
                uploadStatus.style.display = 'none';
                document.getElementById('upload_error').style.display = 'block';
            } else {
                window.setTimeout(function() { 
                    uploadStatus.style.display = 'none';
                    document.getElementById('upload_success').style.display = 'block';
                    var showLink = document.getElementById('show_link');
                    showLink.href = response.path;
                    showLink.innerHTML = getFileName();
                }, 250); 
            }
        };

        // Attach the `startProgress` function to the form submit and show/hide the buttons and status.
        var uploadButtons = document.getElementById('upload_buttons');
        var uploadForm = document.getElementById('upload_form');
        if (uploadForm != null && undefined !== uploadForm) {
            uploadForm.onsubmit = function() {
                uploadButtons.style.display = 'none';
                uploadStatus.style.display = 'block';
                startProgress();
                return true;    
            };
        }
    }
        
    // Check if window supports `onload` and attach the `pageLoad` function.
    if (window.attachEvent) {
        window.attachEvent('onload', pageLoad);
    } else {
        if (window.onload) {
            var onLoad = window.onload;
            window.onload = function() {
                onLoad();
                pageLoad();
            };
        } else {
            window.onload = pageLoad;
        }
    }    
})();

