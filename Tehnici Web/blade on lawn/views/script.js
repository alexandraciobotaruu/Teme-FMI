function createGalleryCarousel(galleryId) {
    const carousel = document.querySelector(`#${galleryId} .carousel`);
    const firstImg = carousel.querySelectorAll("img")[0];
    const arrowIcons = document.querySelectorAll(`#${galleryId} .wrapper i`);
  
    let isDragStart = false,
      isDragging = false,
      prevPageX,
      prevScrollLeft,
      positionDiff;
  
    const showHideIcons = () => {
      let scrollWidth = carousel.scrollWidth - carousel.clientWidth;
      arrowIcons[0].style.display = carousel.scrollLeft == 0 ? "none" : "block";
      arrowIcons[1].style.display =
        carousel.scrollLeft == scrollWidth ? "none" : "block";
    };
  
    arrowIcons.forEach((icon) => {
      icon.addEventListener("click", () => {
        let firstImgWidth = firstImg.clientWidth + 14;
        carousel.scrollLeft += icon.id == `left${galleryId}` ? -firstImgWidth : firstImgWidth;
        setTimeout(() => showHideIcons(), 60);
      });
    });
  
    const autoSlide = () => {
      if (carousel.scrollLeft == carousel.scrollWidth - carousel.clientWidth)
        return;
  
      positionDiff = Math.abs(positionDiff);
      let firstImgWidth = firstImg.clientWidth + 14;
      let valDifference = firstImgWidth - positionDiff;
  
      if (carousel.scrollLeft > prevScrollLeft) {
        return carousel.scrollLeft +=
          positionDiff > firstImgWidth / 3 ? valDifference : -positionDiff;
      }
  
      carousel.scrollLeft -=
        positionDiff > firstImgWidth / 3 ? valDifference : -positionDiff;
    };
  
    const dragStart = (e) => {
      isDragStart = true;
      prevPageX = e.pageX || e.touches[0].pageX;
      prevScrollLeft = carousel.scrollLeft;
    };
  
    const dragging = (e) => {
      if (!isDragStart) return;
      e.preventDefault();
      isDragging = true;
      carousel.classList.add("dragging");
      positionDiff = (e.pageX || e.touches[0].pageX) - prevPageX;
      carousel.scrollLeft = prevScrollLeft - positionDiff;
      showHideIcons();
    };
  
    const dragStop = () => {
      isDragStart = false;
      carousel.classList.remove("dragging");
  
      if (isDragging) return;
      isDragging = false;
      autoSlide();
    };
  
    carousel.addEventListener("mousedown", dragStart);
    carousel.addEventListener("touchstart", dragStart);
  
    carousel.addEventListener("mousemove", dragging);
    carousel.addEventListener("touchmove", dragging);
  
    carousel.addEventListener("mouseup", dragStop);
    carousel.addEventListener("mouseleave", dragStop);
    carousel.addEventListener("touchend", dragStop);
  }
  
  // Call the function for each gallery
  createGalleryCarousel("gallery6");
  createGalleryCarousel("gallery5");
  createGalleryCarousel("gallery4");
  createGalleryCarousel("gallery3");
  createGalleryCarousel("gallery2");
  createGalleryCarousel("gallery1");
  createGalleryCarousel("gallery0");
  











/*
// Gallery 0 

const carousel = document.querySelector(".carousel"),
firstImg = carousel.querySelectorAll("img")[0];
arrowIcons = document.querySelectorAll(".wrapper i");

let isDragStart = false, isDragging = false, prevPageX, prevScrollLeft, positionDiff;



const showHideIcons = () => {
    // showing and hiding prev/next icon according to carousel scroll left value
    let scrollWidth = carousel.scrollWidth - carousel.clientWidth; //getting max scrollable width
    arrowIcons[0].style.display = carousel.scrollLeft == 0 ? "none" : "block"; 
    arrowIcons[1].style.display = carousel.scrollLeft == scrollWidth ? "none" : "block"; 
}

arrowIcons.forEach(icon => {
    icon.addEventListener("click",() => {
        let firstImgWidth =  firstImg.clientWidth + 14; //getting first img width & adding 14 margin value
        // if clicked icon is left, reduce width value from the carousel scroll left else add to it
        carousel.scrollLeft += icon.id == "left" ? -firstImgWidth : firstImgWidth;
        setTimeout(() =>  showHideIcons(), 60); //calling showHideIcon after 60ms
    });
});

const autoSlide = () => {
    // if there is no image left to scroll then return from here
    if(carousel.scrollLeft == (carousel.scrollWidth - carousel.clientWidth)) return;

    positionDiff = Math.abs(positionDiff); // making positionDiff value to positive
    let firstImgWidth = firstImg.clientWidth + 14;
    //geting difference value that needs to add or reduce from carousel left to take middle img center
    let valDifference = firstImgWidth - positionDiff;

    if(carousel.scrollLeft > prevScrollLeft){ //if user us scrolling to the right
        return carousel.scrollLeft += positionDiff > firstImgWidth / 3 ? valDifference : -positionDiff; 
    }
    //if user us scrolling to the left
    carousel.scrollLeft -= positionDiff > firstImgWidth / 3 ? valDifference : -positionDiff;
}

const dragStart = () => {
    // updating global variables value on mouse down event
    isDragStart = true;
    prevPageX = e.pageX || e.touches[0].pageX;
    prevScrollLeft = carousel.scrollLeft;
}
const dragging = (e) => {
    if (!isDragStart) return;
    e.preventDefault();
    isDragging = true;
    carousel.classList.add("dragging");
    positionDiff = (e.pageX || e.touches[0].pageX ) - prevPageX;
    carousel.scrollLeft = prevScrollLeft - positionDiff;
    showHideIcons();
}
const dragStop = () => {
    isDragStart = false;
    carousel.classList.remove("dragging");

    if(isDragging) return;
    isDragging = false;
    autoSlide();
}
carousel.addEventListener("mousedown", dragStart);
carousel.addEventListener("touchstart", dragStart);

carousel.addEventListener("mousemove", dragging);
carousel.addEventListener("touchmove", dragging);

carousel.addEventListener("mouseup", dragStop);
carousel.addEventListener("mouseleave", dragStop);
carousel.addEventListener("touchend", dragStop);

*/
/*
/// carousel 1
const carousel1 = document.querySelector(".carousel1");
const firstImg1 = carousel1.querySelectorAll("img")[0];
const arrowIcons1 = document.querySelectorAll(".wrapper1 i");
let isDragStart1 = false, isDragging1 = false, prevPageX1, prevScrollLeft1, positionDiff1;


// Galerie carousel 1
const showHideIcons1 = () => {
  // Afiseaza sau ascunde iconitele anterioare si urmatoare in functie de pozitia de scroll a carouselului 1
  let scrollWidth1 = carousel1.scrollWidth - carousel1.clientWidth;
  arrowIcons1[0].style.display = carousel1.scrollLeft == 0 ? "none" : "block";
  arrowIcons1[1].style.display = carousel1.scrollLeft == scrollWidth1 ? "none" : "block";
};

arrowIcons1.forEach(icon => {
  icon.addEventListener("click", () => {
    let firstImgWidth1 = firstImg1.clientWidth + 14;
    carousel1.scrollLeft += icon.id == "left1" ? -firstImgWidth1 : firstImgWidth1;
    setTimeout(() => showHideIcons1(), 60);
  });
});

const autoSlide1 = () => {
  if (carousel1.scrollLeft == (carousel1.scrollWidth - carousel1.clientWidth)) return;
  positionDiff1 = Math.abs(positionDiff1);
  let firstImgWidth1 = firstImg1.clientWidth + 14;
  let valDifference1 = firstImgWidth1 - positionDiff1;
  if (carousel1.scrollLeft > prevScrollLeft1) {
    return carousel1.scrollLeft += positionDiff1 > firstImgWidth1 / 3 ? valDifference1 : -positionDiff1;
  }
  carousel1.scrollLeft -= positionDiff1 > firstImgWidth1 / 3 ? valDifference1 : -positionDiff1;
};

const dragStart1 = (e) => {
  isDragStart1 = true;
  prevPageX1 = e.pageX || e.touches[0].pageX;
  prevScrollLeft1 = carousel1.scrollLeft;
};

const dragging1 = (e) => {
  if (!isDragStart1) return;
  e.preventDefault();
  isDragging1 = true;
  carousel1.classList.add("dragging");
  positionDiff1 = (e.pageX || e.touches[0].pageX) - prevPageX1;
  carousel1.scrollLeft = prevScrollLeft1 - positionDiff1;
  showHideIcons1();
};

const dragStop1 = () => {
  isDragStart1 = false;
  carousel1.classList.remove("dragging");

  if (isDragging1) return;
  isDragging1 = false;
  autoSlide1();
};

carousel1.addEventListener("mousedown", dragStart1);
carousel1.addEventListener("touchstart", dragStart1);
carousel1.addEventListener("mousemove", dragging1);
carousel1.addEventListener("touchmove", dragging1);
carousel1.addEventListener("mouseup", dragStop1);
carousel1.addEventListener("mouseleave", dragStop1);
carousel1.addEventListener("touchend", dragStop1);

*/