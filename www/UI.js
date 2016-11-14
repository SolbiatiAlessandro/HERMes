function main() {

  //EFFETTI PAGINA

  

	$(".navbar a, footer a[href='#myPage']").on('click', function(event) {
		if (this.hash !== "") {
    	event.preventDefault();
    	var hash = this.hash;
    
    	$('html, body').animate({scrollTop: $(hash).offset().top}, 900, 
    		function(){
       			window.location.hash = hash;
      		});
    	}
	});

	$(window).scroll(function() {
  		$(".slideanim").each(function(){
   			var pos = $(this).offset().top;
    		var winTop = $(window).scrollTop();
    		if (pos < winTop + 600) {
      			$(this).addClass("slide");
    		}
  		});
	});

  //DONUT REACTIVE

  $('#my_positions').on('mouseenter',function(){
    console.log("updating titles values");
    var non_pos_titoli=document.getElementById("non_pos_titoli").innerHTML;
    var ups_titoli=document.getElementById("ups_titoli").innerHTML;
    var downs_titoli=document.getElementById("downs_titoli").innerHTML;
    
    console.log("non_pos_titoli");
    console.log(non_pos_titoli);
    console.log("ups_titoli");
    console.log(ups_titoli);
    console.log("downs_titoli");
    console.log(downs_titoli);
    
    arr_titoli=non_pos_titoli.split(" ");
    arr_ups=ups_titoli.split(" ");
    arr_downs=downs_titoli.split(" ");

    Morris.Donut({
    element: 'portfolio_donut',
    data: [
      {label: "Long Positions ", value: arr_ups.length},
      {label: "Short Positions", value: arr_downs.length},
      {label: "Not Positioned", value: arr_titoli.length}
      ]

    


    });
    document.getElementById("portfolio_donut").fadeIn(300);
  }) 


  console.log("UI HERMes daily check : console log");
  
  //CODICE PER DAILY CHECK

  var Start_HERMes=$('#Start_HERMes');
  var Display_Outcome=$('#Display_Outcome');
  var Display_Outcome_js=document.getElementById("Display_Outcome");

  Start_HERMes.on('click',function(){
    //parte a calcolare
    console.log("calculating values");
    var non_pos_titoli=document.getElementById("non_pos_titoli").innerHTML;
    var ups_titoli=document.getElementById("ups_titoli").innerHTML;
    var downs_titoli=document.getElementById("downs_titoli").innerHTML;
    
    console.log("non_pos_titoli");
    console.log(non_pos_titoli);
    console.log("ups_titoli");
    console.log(ups_titoli);
    console.log("downs_titoli");
    console.log(downs_titoli);
    
    arr_titoli=non_pos_titoli.split(" ");
    arr_ups=ups_titoli.split(" ");
    arr_downs=downs_titoli.split(" ");
    
    Display_Outcome_js.getAttributeNode("class").value = "btn btn-primary active my_btn";
  }); 

  Display_Outcome.on('click',function(event){
   
    //legge valori da Server.R
   
    console.log("reading daily_check_val");
    var daily_check_index = document.getElementById("HERMes_output_index").innerHTML;
    var daily_check_content = document.getElementById("HERMes_output_content").innerHTML;
    daily_check_content=daily_check_content.split(" ");
    daily_check_index=daily_check_index.split(" ");

    // vengono scritte in output un div con gli indici della lunghezza degli array (daily_check_index) 
    // e un div coi valori, rispett {non_pos} {ups} {downs} in (daily_check_content)

    console.log("daily_check_index");
    console.log(daily_check_index);
    console.log("daily_check_content");
    console.log(daily_check_content);
    console.log(typeof daily_check_content);

    var non_pos_num = Number(daily_check_index[0]);
    var ups_num = Number(daily_check_index[1]);
    var downs_num = Number(daily_check_index[2]);

    var non_pos_outcome = daily_check_content.slice(0,(non_pos_num+1));
    var ups_outcome= daily_check_content.slice((non_pos_num+1),(non_pos_num+ups_num+1));
    var downs_outcome= daily_check_content.slice((non_pos_num+ups_num+1),(non_pos_num+ups_num+downs_num));

    console.log("non_pos_outcome");
    console.log(non_pos_outcome);
    console.log("ups_outcome");
    console.log(ups_outcome);
    console.log("downs_outcome");
    console.log(downs_outcome);

    //output from daily_Check

    // tab non_pos
    for (var i = 0; i < non_pos_num; i++) {
    
      var new_row = document.createElement("TR");
      var new_title = document.createElement("TD");
      var new_outcome = document.createElement("TD");
      var new_scheck = document.createElement("TD");
      new_title.setAttribute("class","tab_text");
      new_outcome.setAttribute("class","tab_text");
      
      new_title.innerHTML = arr_titoli[i];
      if(non_pos_outcome[i]==0) new_outcome.innerHTML = "no signal";
      else if(non_pos_outcome[i]==0.5){ 
        new_outcome.innerHTML = "signal UP";
        new_outcome.setAttribute("style","text-align:right;");
      }
      else if(non_pos_outcome[i]==-0.5){
        new_outcome.innerHTML = "signal DOWN";
        new_outcome.setAttribute("style","text-align:right;");
      }

      //creazione del link
      var icon = document.createElement("SPAN");
      var link_div = document.createElement("SPAN");
      link_div.innerHTML=" SingleCheck";
      icon.setAttribute('class',"glyphicon glyphicon-circle-arrow-right");
      var link = document.createElement("A");
      link.setAttribute('href','#scheck');
      link.setAttribute('class','btn');
      link.setAttribute('style',"color: black !important");

      //enable the link to go to the SingleCheck
      var onClick_arg1 = "toSingleCheck(\"";
      var onClick_arg2 = "\",";
      var onClick_arg3 = ")";
      link.setAttribute('onClick',onClick_arg1+arr_titoli[i]+onClick_arg2+non_pos_outcome[i]+onClick_arg3);

      link.appendChild(icon);
      link.appendChild(link_div); 
      new_scheck.appendChild(link);

      new_row.appendChild(new_title);
      new_row.appendChild(new_outcome);
      new_row.appendChild(new_scheck);

      var output_table_body = document.getElementById("output_table_body_nyp");
      output_table_body.appendChild(new_row);

    }

    for (var i = 0; i < ups_num; i++) {
    
      var new_row = document.createElement("TR");
      var new_title = document.createElement("TD");
      var new_outcome = document.createElement("TD");
      var new_scheck = document.createElement("TD");
      new_title.setAttribute("class","tab_text");
      new_outcome.setAttribute("class","tab_text");
      
      new_title.innerHTML = arr_ups[i];
      if(non_pos_outcome[i]==0) new_outcome.innerHTML = "position unvaried";
      else if(ups_outcome[i]==-1) new_outcome.innerHTML = "quit position signal";

        //creazione del link
      var icon = document.createElement("SPAN");
      var link_div = document.createElement("SPAN");
      link_div.innerHTML=" SingleCheck";
      icon.setAttribute('class',"glyphicon glyphicon-circle-arrow-right");
      var link = document.createElement("A");
      link.setAttribute('href','#scheck');
      link.setAttribute('class','btn');
      link.setAttribute('style',"color: black !important");
      var onClick_arg1 = "toSingleCheck(\"";
      var onClick_arg2 = "\",";
      var onClick_arg3 = ")";
      link.setAttribute('onClick',onClick_arg1+arr_ups[i]+onClick_arg2+"3"+onClick_arg3);


      link.appendChild(icon);
      link.appendChild(link_div);
      new_scheck.appendChild(link);

      new_row.appendChild(new_title);
      new_row.appendChild(new_outcome);
      new_row.appendChild(new_scheck);

      var output_table_body_ups = document.getElementById("output_table_body_ups");
      output_table_body_ups.appendChild(new_row);
    }

    for (var i = 0; i < downs_num; i++) {
    
      var new_row = document.createElement("TR");
      var new_title = document.createElement("TD");
      var new_outcome = document.createElement("TD");
      var new_scheck = document.createElement("TD");
      new_title.setAttribute("class","tab_text");
      new_outcome.setAttribute("class","tab_text");
      
      new_title.innerHTML = arr_downs[i];
      if(downs_outcome[i]==0) new_outcome.innerHTML = "position unvaried";
      else if(downs_outcome[i]==1) new_outcome.innerHTML = "quit position signal";

      //creazione del link
      var icon = document.createElement("SPAN");
      var link_div = document.createElement("SPAN");
      link_div.innerHTML=" SingleCheck";
      icon.setAttribute('class',"glyphicon glyphicon-circle-arrow-right");
      var link = document.createElement("A");
      link.setAttribute('href','#scheck');
      link.setAttribute('class','btn');
      link.setAttribute('style',"color: black !important");
      var onClick_arg1 = "toSingleCheck(\"";
      var onClick_arg2 = "\",";
      var onClick_arg3 = ")";
      link.setAttribute('onClick',onClick_arg1+arr_downs[i]+onClick_arg2+"4"+onClick_arg3);

      link.appendChild(icon);
      link.appendChild(link_div);

      new_scheck.appendChild(link); 

      new_row.appendChild(new_title);
      new_row.appendChild(new_outcome);
      new_row.appendChild(new_scheck);

      var output_table_body_downs = document.getElementById("output_table_body_downs");
      output_table_body_downs.appendChild(new_row);

    }
    
  });

  //SINGLE CHECK

  //sliding bar
  $("#ex6").slider();
  $("#ex6").on("slide", function(slideEvt) {
    $("#ex6SliderVal").text(slideEvt.value);
  });

  /*
  NOT WORKING WITH REACTIVITY
  
  toggle_boxes
  var alg_oc = $('#alg_oc');
  var rm_oc = $('#rm_oc');
  var toggle_alg_oc = $('#toggle_alg_oc');
  var toggle_rm_oc = $('#toggle_rm_oc');

  alg_oc.hide();
  rm_oc.hide();
  toggle_alg_oc.on('click',function(){
    alg_oc.slideToggle(500);
  })
  toggle_rm_oc.on('click',function(){
    rm_oc.slideToggle(500);
  })*/

  //Capital progress bar

  $('.skill').on('click', 'button', function(){
    var skillBar = $(this).siblings().find('.inner');
    var skillVal = 100*((document.getElementById("cap_position").innerHTML)/1000);
    console.log(skillVal);
    var skillVal = skillVal.toString();
    var skillVal = skillVal+"%";
    console.log(skillVal);
    $(skillBar).animate({
        height: skillVal
    }, 1500);
  });



}

function toSingleCheck(my_name,my_outcome){
  console.log(my_outcome);
  document.getElementById("scheck_title").innerHTML="Title name: "+my_name;
  if(my_outcome==0.5) 
    document.getElementById("scheck_signal").innerHTML="Algorithm signal: UP signal";
  else if(my_outcome==-0.5)
    document.getElementById("scheck_signal").innerHTML="Algorithm signal: DOWN signal";
  else if(my_outcome==0)
    document.getElementById("scheck_signal").innerHTML="Algorithm signal: no signal";
  else if(my_outcome==3)
    document.getElementById("scheck_signal").innerHTML="currently positioned as: Long Position";
  else if(my_outcome==4)
    document.getElementById("scheck_signal").innerHTML="currently positioned as: Short Position";
}




$(document).ready(main);