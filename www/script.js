
$(function() {
  
  document.getElementById('ss-connect-dialog').setAttribute("role", "dialog");
  
  document.body.removeAttribute('style');
  document.getElementsByTagName('html')[0].removeAttribute('style');
  
  $(document).on("shiny:disconnected", function(event) {

    // by only finding elements that do not have tabindex="-1" we ensure we don't
    // corrupt the previous state of the element if a modal was already open
    nodes = document.querySelectorAll('body *');
  
    for (var i = 0; i < nodes.length; i++) {
      var node = nodes[i];
  
      node.setAttribute('tabindex', -1);
      node.setAttribute('aria-hidden', 'true');
      node.setAttribute('disabled', '');

      // tabindex=-1 does not prevent the mouse from focusing the node (which
      // would show a focus outline around the element). prevent this by disabling
      // outline styles while the modal is open
      // @see https://www.sitepoint.com/when-do-elements-take-the-focus/
      node.style.outline = 'none';
      
    }
    
    document.getElementById('ss-connect-dialog').setAttribute('aria-hidden', 'false');
    document.getElementById('ss-connect-dialog').removeAttribute('disabled');
    document.getElementById('ss-connect-dialog').removeAttribute('tabindex');
    document.getElementById('ss-reload-link').setAttribute('aria-hidden', 'false');
    document.getElementById('ss-reload-link').removeAttribute('disabled');
    document.getElementById('ss-connect-dialog').focus();
    document.getElementById('ss-reload-link').removeAttribute('tabindex');
    
    }
  )
  
  $(document).on("shiny:busy", function(event) {
    const keyboardfocusableElements = document.querySelectorAll(
      'a, button, input, select'
    );
    if ($('#id-map').length) {
      
      const map = document.getElementById('id-map');
      map.setAttribute('tabindex', -1);
      const mapElements = document.getElementById('id-map').getElementsByClassName('leaflet-marker-pane')[0];
      const mapfocusableElements = mapElements.querySelectorAll(
        'div, img'
      );
      for (let i=0; i < mapfocusableElements.length; i++){
        mapfocusableElements[i].setAttribute('tabindex', -1);
      }
    }
    
    
    for (let i=0; i < keyboardfocusableElements.length; i++){
      if(keyboardfocusableElements[i].tagName == 'A') {
        keyboardfocusableElements[i].setAttribute('tabindex', -1);
        keyboardfocusableElements[i].blur();
      }
      else {
        keyboardfocusableElements[i].disabled = true;
        keyboardfocusableElements[i].blur();
      }
    }
  })
  
  $(document).on("shiny:idle", function(event) {
    const keyboardfocusableElements = document.querySelectorAll(
      'a, button, input, select'
    );
    
    if ($('#id-map').length) {
      const map = document.getElementById('id-map');
      map.setAttribute('tabindex', 0);
      const mapElements = document.getElementById('id-map').getElementsByClassName('leaflet-marker-pane')[0];
      const mapfocusableElements = mapElements.querySelectorAll(
        'div, img'
      );
      for (let i=0; i < mapfocusableElements.length; i++){
        mapfocusableElements[i].setAttribute('tabindex', 0);
      }
    }
    
    for (let i=0; i < keyboardfocusableElements.length; i++){
      
      if(keyboardfocusableElements[i].tagName == 'A') {
        keyboardfocusableElements[i].removeAttribute('tabindex');
      }
      else {
        keyboardfocusableElements[i].disabled = false;
      }
    }
  })
  
});

