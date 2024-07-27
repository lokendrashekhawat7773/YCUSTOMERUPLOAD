CLASS zcl_cust_upload DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.


    DATA:lt_je_deep TYPE TABLE FOR ACTION IMPORT i_journalentrytp~post,
         wa_je_deep LIKE LINE OF lt_je_deep,
         w2         LIKE LINE OF wa_je_deep-%param-_withholdingtaxitems,
         w3         LIKE LINE OF w2-_currencyamount,
         w_profit   LIKE LINE OF wa_je_deep-%param-_glitems.


    CLASS-DATA:gl_item        LIKE wa_je_deep-%param-_glitems[],
               ar_item        LIKE wa_je_deep-%param-_aritems[],
               ap_item        LIKE wa_je_deep-%param-_apitems[],
               taxitems       LIKE wa_je_deep-%param-_taxitems[],
               with_hold_item LIKE wa_je_deep-%param-_withholdingtaxitems[].



    TYPES : BEGIN OF ty_custitem,

              srno             TYPE i,
              company(4)       TYPE c,
              customer(10)     TYPE c,
              postingdate(10)  TYPE c,
              documentdate(10) TYPE c,
              reference(10)    TYPE c,
              amount           TYPE i_operationalacctgdocitem-amountintransactioncurrency,
              csgstamt         TYPE i_operationalacctgdocitem-amountintransactioncurrency,
              igstamt          TYPE i_operationalacctgdocitem-amountintransactioncurrency,
              currency         TYPE i_operationalacctgdocitem-transactioncurrency,
              businessplace    TYPE i_operationalacctgdocitem-businessplace,
              documenttype     TYPE i_operationalacctgdocitem-accountingdocumenttype,
              spgl             TYPE i_operationalacctgdocitem-glaccount,
              alternategl      TYPE i_operationalacctgdocitem-glaccount,
              taxcode          TYPE i_operationalacctgdocitem-taxcode,
              baselinedate(10) TYPE c,
              tdstype(2)       TYPE c,
              tdscode          TYPE i_operationalacctgdocitem-taxcode,
              tdsbase          TYPE i_operationalacctgdocitem-amountintransactioncurrency,
              itemtext(50)     TYPE c,
              glaccount        TYPE i_operationalacctgdocitem-glaccount,
              costcenter       TYPE i_operationalacctgdocitem-costcenter,
              profitcenter     TYPE i_operationalacctgdocitem-profitcenter,
              plant            TYPE i_operationalacctgdocitem-plant,
              wbs              TYPE i_operationalacctgdocitem-wbselementinternalid,
              product          TYPE i_operationalacctgdocitem-product,
              qty              TYPE i_operationalacctgdocitem-quantity,
              baseunit         TYPE i_operationalacctgdocitem-baseunit,

            END OF ty_custitem .

    CLASS-DATA:
      itcust_item TYPE TABLE OF ty_custitem,
      wacust_item TYPE ty_custitem.

    CLASS-DATA:
      lv_cid     TYPE abp_behv_cid,
      i_responce TYPE TABLE OF string.

    CLASS-DATA:doc       TYPE string,
               error     TYPE string,
               responce1 TYPE string,
               item(6)   TYPE n,
               item1(6)  TYPE n.


    INTERFACES if_http_service_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CUST_UPLOAD IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.


    DATA(req) = request->get_form_fields(  ).
    response->set_header_field( i_name = 'Access-Control-Allow-Origin' i_value = '*' ).
    response->set_header_field( i_name = 'Access-Control-Allow-Credentials' i_value = 'true' ).

    DATA(validate) = VALUE #( req[ name = 'validate' ]-value OPTIONAL ) .

    DATA(body)  = request->get_text(  )  .
    xco_cp_json=>data->from_string( body )->write_to( REF #(  itcust_item ) ).

    TRY.
        lv_cid = to_upper( cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ) ).
      CATCH cx_uuid_error.
        ASSERT 1 = 0.
    ENDTRY.

    DATA totamt TYPE p    DECIMALS 2 .
    DATA(user) = sy-uname .

    DATA : tax_amt TYPE i_operationalacctgdocitem-amountintransactioncurrency.
    DATA : tot_tax_amt TYPE i_operationalacctgdocitem-amountintransactioncurrency.
    SELECT * FROM ytax_code2_tab AS a FOR ALL ENTRIES IN @itcust_item WHERE a~taxcode = @itcust_item-taxcode
     INTO TABLE @DATA(it_gstrate) .

    LOOP AT itcust_item ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-businessplace        = |{ <fs>-businessplace ALPHA = IN }|.
      <fs>-spgl          = |{ <fs>-spgl ALPHA = IN }|.
      <fs>-alternategl   = |{ <fs>-alternategl ALPHA = IN }|.
      <fs>-glaccount     = |{ <fs>-glaccount ALPHA = IN }|.
      <fs>-costcenter    = |{ <fs>-costcenter ALPHA = IN }|.
      <fs>-profitcenter  = |{ <fs>-profitcenter ALPHA = IN }|.
      <fs>-product       = |{ <fs>-product ALPHA = IN }|.
      <fs>-wbs           = |{ <fs>-wbs ALPHA = IN }|.
      READ TABLE it_gstrate ASSIGNING FIELD-SYMBOL(<wa1>) WITH KEY taxcode = <fs>-taxcode .
      IF sy-subrc = 0 .
        IF <wa1>-condition_type = 'JOIG' OR <wa1>-condition_type = 'JIIG' .
          <fs>-igstamt = ( <fs>-amount * <wa1>-gstrate ) / 100 .
        ELSEIF <wa1>-condition_type = 'JOCG' OR <wa1>-condition_type = 'JICG'
            OR <wa1>-condition_type = 'JOSG' OR <wa1>-condition_type = 'JISG' .
          <fs>-csgstamt = ( <fs>-amount * <wa1>-gstrate ) / 100 .
        ENDIF.
      ENDIF .
      MODIFY itcust_item FROM <fs> .
    ENDLOOP.



    LOOP AT itcust_item ASSIGNING FIELD-SYMBOL(<fs2>).




      " 05/27/2024
      DATA(posting_dt)  = <fs2>-postingdate+6(4)  && <fs2>-postingdate+3(2)  && <fs2>-postingdate+0(2).
      DATA(document_dt) = <fs2>-documentdate+6(4)  && <fs2>-documentdate+3(2)  && <fs2>-documentdate+0(2).
      DATA(baselinedate) = <fs2>-baselinedate+6(4)  && <fs2>-baselinedate+3(2)  && <fs2>-baselinedate+0(2).

      wa_je_deep-%cid   = lv_cid.
      wa_je_deep-%param = VALUE #( companycode                  = <fs2>-company
                                   createdbyuser                = user
                                   documentreferenceid          = <fs2>-reference
                                   businesstransactiontype      = 'RFBU'
                                   accountingdocumenttype       = <fs2>-documenttype
                                   documentdate                 = document_dt
                                   postingdate                  = posting_dt
                                   taxdeterminationdate         = baselinedate

                                                                                      ).
      DATA : amount TYPE string.

      item = item + 1.
      item1 = item1 + 1.
      gl_item =  VALUE #( ( glaccountlineitem             = item
                            taxitemacctgdocitemref        = item1
                            glaccount                     = <fs2>-glaccount
*                            reference2idbybusinesspartner = <fs2>-customer
                            taxcode                       = <fs2>-taxcode
                            businessplace                 = <fs2>-company
                            costcenter                    = <fs2>-costcenter
                            profitcenter                  = <fs2>-profitcenter
                            plant                         = <fs2>-plant
                            wbselement                    = COND #( WHEN <fs2>-wbs IS INITIAL
                                                              THEN '' ELSE <fs2>-wbs    ) "'X'
                            material                      = <fs2>-product
                            quantity                      = <fs2>-qty
                            baseunit                      = <fs2>-baseunit
                            documentitemtext              = <fs2>-itemtext
                            _currencyamount               = VALUE #( ( currencyrole           = '00'
                                                            journalentryitemamount = <fs2>-amount
                                                            currency               = <fs2>-currency   ) )
                            _profitabilitysupplement     = VALUE #(   profitcenter  = <fs2>-profitcenter
                                                                      costcenter    = <fs2>-costcenter
                                                                      soldproduct   = <fs2>-product   )
                                                                                                  ) ).


      .
      APPEND LINES OF gl_item TO wa_je_deep-%param-_glitems.




      LOOP AT it_gstrate ASSIGNING FIELD-SYMBOL(<wa_rate1>) WHERE taxcode = <fs2>-taxcode.
        tax_amt = ( <fs>-amount * <wa_rate1>-gstrate ) / 100 .
        tot_tax_amt = tot_tax_amt + tax_amt.
        amount = <fs2>-amount.
        IF amount CS '-'.
        tax_amt = tax_amt * -1.
        ENDIF.
        item = item + 1.

        taxitems =  VALUE #( (
                             glaccountlineitem      = item
                             taxcode                = <fs2>-taxcode
                             conditiontype          = <wa_rate1>-condition_type
                             taxitemacctgdocitemref = item1
                             taxdeterminationdate   = posting_dt
                             isdirecttaxposting     = 'X'
                             _currencyamount        = VALUE #( ( currencyrole           = '00'
                                                                 journalentryitemamount =  tax_amt
                                                                 taxbaseamount          =  <fs2>-amount
                                                                 currency               = 'INR' ) ) ) ).
        APPEND LINES OF taxitems  TO wa_je_deep-%param-_taxitems.
        CLEAR tax_amt.

      ENDLOOP.



      item = item + 1.
      amount = <fs2>-amount.
      IF amount CS '-'.
        <fs2>-amount = <fs2>-amount * -1.
        <fs2>-amount = <fs2>-amount + tot_tax_amt.
*        <fs2>-amount = <fs2>-amount * -1.
        CLEAR totamt.
      ELSE.
        <fs2>-amount = <fs2>-amount + tot_tax_amt.
        <fs2>-amount = <fs2>-amount * -1 .
      ENDIF.
      CLEAR amount.

      ar_item =  VALUE #( (
                                  glaccountlineitem  = item
                                  specialglcode      = <fs2>-spgl
                                  glaccount          = <fs2>-alternategl
                                  taxcode            = <fs2>-taxcode
                                  duecalculationbasedate     = baselinedate

                                  customer           = <fs2>-customer
                                  businessplace      = <fs2>-businessplace
*                                  reference2idbybusinesspartner = <fs2>-customer
                                  documentitemtext              = <fs2>-itemtext
                                  _currencyamount    = VALUE #( ( currencyrole           = '00'
                                                                  journalentryitemamount = <fs2>-amount
*                                                                  journalentryitemamount = totamt * ( -1 )
                                                                  currency               = <fs2>-currency ) ) ) ).
      APPEND LINES OF ar_item  TO wa_je_deep-%param-_aritems.




*adding customerar item"""""""""""

      IF  <fs2>-tdscode IS NOT INITIAL .
        with_hold_item =  VALUE #( (
                        glaccountlineitem  = item
                        withholdingtaxcode = <fs2>-tdscode
                        withholdingtaxtype = <fs2>-tdstype

*                            WhldgTaxBaseIsEnteredManually = ( it_data-tdsbase *  -1 )
                        _currencyamount    = VALUE #( ( currencyrole           = '00'
                                                        taxbaseamount          = <fs2>-tdsbase
                                                        currency               = <fs2>-currency    ) )
                                                                                                        ) ).
        APPEND LINES OF with_hold_item  TO wa_je_deep-%param-_withholdingtaxitems .
      ENDIF.


*CLEAR: item,item1 .
      CLEAR : totamt , tot_tax_amt.

*    ENDLOOP.

      APPEND wa_je_deep TO lt_je_deep .
      MODIFY ENTITIES OF i_journalentrytp
    ENTITY journalentry
    EXECUTE post FROM lt_je_deep
    FAILED DATA(ls_failed_deep)
    REPORTED DATA(ls_reported_deep)
    MAPPED DATA(ls_mapped_deep).

      CLEAR : lt_je_deep.


      IF ls_failed_deep IS NOT INITIAL.
        LOOP AT ls_reported_deep-journalentry ASSIGNING FIELD-SYMBOL(<ls_reported_deep>).
          IF sy-tabix <> 1.
            IF <ls_reported_deep>-%msg->if_t100_dyn_msg~msgty = 'E'.
              DATA(lv_result) = <ls_reported_deep>-%msg->if_message~get_longtext( ).
              CONCATENATE '$$$$ Error :-' lv_result INTO responce1 .
              APPEND responce1 TO i_responce.
              CLEAR responce1.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ELSE.
        COMMIT ENTITIES BEGIN
        RESPONSE OF i_journalentrytp

        FAILED DATA(lt_commit_failed)
        REPORTED DATA(lt_commit_reported).
        ...
        COMMIT ENTITIES END.
        LOOP AT lt_commit_reported-journalentry INTO DATA(w).
          IF w-%msg->if_t100_dyn_msg~msgty = 'S'.
            responce1  = |$$$$ Document :-{ w-%msg->if_t100_dyn_msg~msgv2+0(10) } Generated|.
            APPEND responce1 TO i_responce.
            CLEAR responce1.
          ENDIF.
        ENDLOOP.

      ENDIF.
    ENDLOOP.
    FREE : lt_je_deep[].
    DATA:json TYPE REF TO if_xco_cp_json_data.
    CLEAR:responce1 .

    xco_cp_json=>data->from_abap(
      EXPORTING
        ia_abap      = i_responce
      RECEIVING
        ro_json_data = json   ).
    json->to_string(
      RECEIVING
        rv_string =  responce1 ).




    response->set_text( responce1 ).




  ENDMETHOD.
ENDCLASS.
