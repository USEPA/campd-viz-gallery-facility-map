
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
    if ($('#map').length) {
      
      const map = document.getElementById('map');
      map.setAttribute('tabindex', -1);
      const mapElements = document.getElementById('map').getElementsByClassName('leaflet-marker-pane')[0];
      if (typeof mapElements !== 'undefined') {
        const mapfocusableElements = mapElements.querySelectorAll(
          'div, img'
        );
        for (let i=0; i < mapfocusableElements.length; i++){
          mapfocusableElements[i].setAttribute('tabindex', -1);
        }
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
  
  function enableAllElements() {
    const keyboardfocusableElements = document.querySelectorAll(
      'a, button, input, select'
    );
    
    if ($('#map').length) {
      const map = document.getElementById('map');
      map.setAttribute('tabindex', 0);
      const mapElements = document.getElementById('map').getElementsByClassName('leaflet-marker-pane')[0];
      if (typeof mapElements !== 'undefined') {
        const mapfocusableElements = mapElements.querySelectorAll(
          'div, img'
        );
        for (let i=0; i < mapfocusableElements.length; i++){
          mapfocusableElements[i].setAttribute('tabindex', 0);
        }
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
  }
  
  $(document).on("shiny:idle", function(event) {
    
    const modal = document.getElementById('shiny-modal');
    
    if (modal !== null) {
      
      modal.removeAttribute('tabindex');
      const modalElements = modal.querySelectorAll(
        'a, button'
      );
      
      for (let i=0; i < modalElements.length; i++){
        if(modalElements[i].tagName == 'A') {
          modalElements[i].removeAttribute('tabindex');
        }
        else {
          modalElements[i].disabled = false;
        }
      }
      return;
    }
    
    enableAllElements();
    
  })
  
  const minuselements = [...document.getElementsByClassName("fa-minus")];
  const pluselements = [...document.getElementsByClassName("fa-plus")];
  const questionelements = [...document.getElementsByClassName("fa-question")];
  const downloadelements = [...document.getElementsByClassName("fa-download")];
  const collapseElements = minuselements.concat(pluselements);
  const subsetelements = collapseElements.concat(questionelements);
  const elements = subsetelements.concat(downloadelements);
  
  for (let i=0; i < elements.length; i++){
    elements[i].setAttribute("role", "img");
  }
  
  const facilitySummaryBoxButton = document.getElementById("facility-summary-box").getElementsByClassName("btn-box-tool")[0];
  facilitySummaryBoxButton.insertAdjacentText("beforeend", "button to expand and collapse the facility summary");
  
  const complianceSummaryBoxButton = document.getElementById("compliance-summary-box").getElementsByClassName("btn-box-tool")[0];
  complianceSummaryBoxButton.insertAdjacentText("beforeend", "button to expand and collapse the compliance summary");
  
  const howToUseBoxButton = document.getElementById("how-to-use-map-box").getElementsByClassName("btn-box-tool")[0];
  howToUseBoxButton.insertAdjacentText("beforeend", "button to expand and collapse the 'how to use the map' section");
  const sourceDataBoxButton = document.getElementById("source-data-box").getElementsByClassName("btn-box-tool")[0];
  sourceDataBoxButton.insertAdjacentText("beforeend", "button to expand and collapse the 'source data' section");
  
  
  const formelements = [...document.getElementsByClassName("form-group")];
  
  for (let i=0; i < formelements.length; i++){
    const id = formelements[i].getElementsByTagName("label")[0].textContent;
    formelements[i].getElementsByTagName("select")[0].setAttribute("label", id);
  }
  
  $(document).keyup(function(event) {
      if ($(".leaflet-marker-icon").is(":focus") && (event.key == "Enter")) {
          event.target.click();
      }
  });
  
});

