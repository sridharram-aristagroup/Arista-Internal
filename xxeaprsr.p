/* xxeaprsr.p - Parser program for EasyAccess Inbound Voucher request        */
/* V8:ConvertMode=Maintenance                                                */
/* Program name : xxsosomt.p                                                 */
/* Author       : Sridhar R                                                  */
/* Date         : 15th April 2014                                            */
/* Id           :                                                            */
/* Purpose      : CIM to Voucher Maintenance & Error Report                  */
/* Notes        :                                                            */
/*****************************************************************************/
/*Eco:SR010421 Date: 04 Jan, 2021                        Modified By: Sridhar*/
/*Purpose: To add Invoice Terms & Seaparate Check fields from EA             */
/*****************************************************************************/
/*Eco:SWA  Date: 26 July, 2021                           Modified By: Sridhar*/
/*Purpose: To accept Two Way PO data and create vouchers                     */
/*****************************************************************************/
{mfdtitle.i}

/*Variable definitions for gpglef.p*/
{gpglefdf.i}
/*For Entity Security*/
{gldydef.i new}
{gldynrm.i new}
{glsec.i}

define variable l_scrnmsg  as character format "x(68)" no-undo.
define variable l_scrnmsg1 as character format "x(68)" no-undo.
define variable l_continue_yn like mfc_logical no-undo.
define variable l_dirpath  as character format "x(50)" no-undo.
define variable l_dirpath2 as character format "x(50)" no-undo.
define variable l_filename as character format "x(50)" no-undo.
define variable hdoc       as handle no-undo.
define variable hroot      as handle no-undo.
define variable hrootchild as handle no-undo.
define variable l_errFile  as character format "x(50)" no-undo.
define variable m_per_yr   like mfc_char               no-undo.
define variable valid-combi as logical no-undo.

define temp-table ttVoucher no-undo
  field ttVLine as integer format ">>9" label "Vchr#"
  field ttBatch like ap_batch
  field ttApRef like vo_ref
  field ttVend like ap_vend
  field ttInvoice like vo_invoice
  field ttTotal   like ap_amt
  field ttTotal2  like ap_amt
  field ttEffdate like ap_effdate
  field ttDate    like ap_date
  field ttvocrterms  like vo_cr_terms /*SR010421*/
  field ttvoseparate like vo_separate /*SR010421*/
  field ttVpopo   like vpo_po
  field ttTwowaypo   as logical  /*SWA*/
  field ttHSev  as   character format "x(8)"  label "Severity"
  field ttHMsg  as   character format "x(75)" label "Message"
  index idx1 ttVend ttInvoice ttEffdate ttDate.

define temp-table ttDetail no-undo
  field ttLink    as integer
  field ttAmt     like vod_amt
  field ttRLine   as integer
  field ttRcvr    like prh_receiver
  field ttPOLine  like prh_line
  field ttTaxable as character
  field ttTax_Usage like pod_tax_usage
  field ttLine as integer  format ">>9" label "Line#"
  field ttAcct    like ap_acct
  field ttSub     like ap_sub
  field ttCc      like ap_cc
  field ttInvqty  like vph_inv_qty
  field ttCurramt like vph_curr_amt
  field ttEntity  like vod_entity
  field ttDesc      as character
  field ttProject like vod_project
  field ttSev  as   character format "x(8)"  label "Severity"
  field ttMsg  as   character format "x(75)" label "Message"
  index idx1 ttLink ttLine ttRLine.

define stream strmdirlist.
define stream strmErrRpt.

FORM
   l_scrnmsg  colon 3 no-label
   l_scrnmsg1 colon 3 no-label
   l_dirpath  colon 3 no-label
   skip(1)
   l_continue_yn colon 25 label "Continue"
   skip
WITH FRAME a SIDE-LABELS WIDTH 80.

/* SET EXTERNAL LABELS */
setFrameLabels(frame a:handle).

assign
   l_dirpath  = "./"
   l_dirpath2 = "./".

for first code_mstr no-lock
   where code_domain = global_domain
   and code_fldname = "XXEAInterfaceExportFolder":
   l_dirpath = code_value + code_cmmt.
end. /*for first code_mstr*/

for first code_mstr no-lock
   where code_domain = global_domain
   and code_fldname ="XXEAInterfaceArchive":
   l_dirpath2 = code_value + code_cmmt.
end. /*for first code_mstr*/

assign
  l_scrnmsg =
  "Directory path for input files is stored in Generalized Codes Maint"
  l_scrnmsg1 =
  "with Field Name = XXEAInterfaceExportFolder. The directory path is ".

display
   l_scrnmsg
   l_scrnmsg1
   l_dirpath
with frame a.

{wbrp01.i}

mainloop:
REPEAT:

   if c-application-mode <> 'web' then
   update
      l_continue_yn
   with frame a.

   {wbrp06.i &command = update &fields = "  l_continue_yn"
      &frm = "a"}

   if (c-application-mode <> 'web') or
      (c-application-mode = 'web' and
      (c-web-request begins 'data')) then do:
      bcdparm = "".
      {mfquoter.i l_continue_yn }
   end.

   IF NOT l_continue_yn THEN UNDO mainloop, RETRY mainloop.

   /* OUTPUT DESTINATION SELECTION */
   {gpselout.i &printType = "printer"
               &printWidth = 132
               &pagedFlag = " "
               &stream = " "
               &appendToFile = " "
               &streamedOutputToTerminal = " "
               &withBatchOption = "yes"
               &displayStatementType = 1
               &withCancelMessage = "yes"
               &pageBottomMargin = 6
               &withEmail = "yes"
               &withWinprint = "yes"
               &defineVariables = "yes"}
   {mfphead.i}

   input stream strmdirlist from os-dir(l_dirpath).
   repeat:
     import stream strmdirlist l_filename.
     l_filename = trim(l_filename).

     if l_filename matches "*.prp" then do:
        empty temp-table ttVoucher no-error.
        empty temp-table ttDetail  no-error.

        create x-document hdoc.
        create x-noderef hroot.
        create x-noderef hrootchild.

        hdoc:load("file", l_dirpath + l_filename, false).
        hdoc:get-document-element(hroot).
        hroot:get-child(hrootchild, 2).

        /*Procedure to parse the input XML file and populate temp-table */
        run getReqDetails
          (input hrootchild).

         /*Procedure for 28.1 Pre-CIM validations */
         RUN procValidate.

         /*Validate Acc/Sub/CC/Proj combination */
         /*Done outside of procValidate as the shared variable in
         {gprunp.i "gpglvpl" "p" "initialize"} cannot be defined inside
         local Procedure procValidate*/
         FOR EACH ttDetail
            WHERE ttSev <> "error"
            AND CAN-FIND(FIRST ttVoucher
                         WHERE ttVline = ttLink
                           AND ttHSev  <> "error") EXCLUSIVE-LOCK:
            /* To neglect Project Code during validations if BLANK */
            IF ttProject = "" THEN
            DO:
              {gprunp.i "gpglvpl" "p" "set_proj_ver" "(input false)"}
            END. /*IF pj = "" */

            /* Initialize Settings */
            {gprunp.i "gpglvpl" "p" "initialize"}

            /* Set Control For Message Display From validate_fullcode*/
            {gprunp.i "gpglvpl" "p" "set_disp_msgs" "(INPUT FALSE)"}

            /* Set Control to validate unconditionally regardless of 36.1 */
            {gprunp.i "gpglvpl" "p" "set_always_ver" "(INPUT NO)"}

            /* Acct/SubAcct/CC/Proj Validation */
            IF ttAcct <> "" THEN DO:
               {gprunp.i "gpglvpl" "p" "validate_fullcode"
                          "(INPUT  ttAcct,
                            INPUT  ttSub,
                            INPUT  ttCc,
                            INPUT  ttProject,
                            OUTPUT valid-combi)"}
               IF NOT valid-combi THEN DO:
                  ASSIGN
                     ttSev = "error"
                     ttMsg = "Invalid Acct/Sub Acct/Cost Center Combination".
                  next.
               END. /*if not valid-combi */
            END. /*IF ttAcct <> ""*/
         END. /*for each ttDetail*/

         /*Procedure to create CIM file and perform CIM on 28.1 */
         RUN procCim.

         l_errFile = substring(l_filename, 1,length(l_filename) - 3) + "txt".

         if l_filename matches "*NonPO*" then
            l_errFile = l_dirpath + "ErrorNonPO" +
                       substring(l_errFile, index(l_errFile, "-")).
         else if l_filename matches "*PO*" then
            l_errFile = l_dirpath + "ErrorPO" +
                       substring(l_errFile, index(l_errFile, "-")).

         /*Procedure to dump the CIM output (temp-table) in pre-defined format*/
         run procErrorReport
            (input l_errFile).

        /* Archive original voucher xml files (.prp) in Archive directory &
            delete from Upload directory */
        os-copy value(l_dirpath + l_filename) value(l_dirpath2).
        os-delete value(l_dirpath + l_filename).

        /* Archive error report files in Archive directory */
        os-copy value(l_errFile) value(l_dirpath2).

        delete object hrootchild.
        delete object hroot.
        delete object hdoc.
     end. /* if l_filename matches "*.prp" */
   end. /* repeat */
   input stream strmdirlist close.

   {mfrtrail.i}
END. /*mainloop*/

{wbrp04.i &frame-spec = a}

procedure getReqDetails:
  define input parameter hcnode as handle no-undo. /*dsVoucher*/

  define variable hvoucher as handle no-undo.
  define variable hvdetail as handle no-undo.
  define variable hnode    as handle no-undo.
  define variable hnode1   as handle no-undo.
  define variable ctr  as integer no-undo.
  define variable ctr1 as integer no-undo.
  define variable ctr2 as integer no-undo.
  define variable ctr3 as integer no-undo.
  define variable lctr  as integer no-undo.
  define variable lctr1 as integer no-undo.
  define variable lctr2 as integer no-undo.

  define variable lVend    like ap_vend no-undo.
  define variable lInvoice like vo_invoice no-undo.
  define variable lAcct    like ap_acct    no-undo.
  define variable lSub     like ap_sub     no-undo.
  define variable lCc      like ap_cc      no-undo.
  define variable lRcvr    like prh_receiver no-undo.
  define variable lPOLine  like prh_line     no-undo.
  define variable lTotal   like ap_amt       no-undo.
  define variable lTotal2  like ap_amt       no-undo.
  define variable lEffdate like ap_effdate   no-undo.
  define variable lDate    like ap_date      no-undo.
  define variable lvocrterms  like vo_cr_terms no-undo. /*SR010421*/
  define variable lvoseparate like vo_separate no-undo. /*SR010421*/
  define variable lEntity  like vod_entity   no-undo.
  define variable lAmt     like vod_amt      no-undo.
  define variable lInvqty  like vph_inv_qty  no-undo.
  define variable lCurramt like vph_curr_amt no-undo.
  define variable lVpopo   like vpo_po       no-undo.
  define variable lDesc    as character      no-undo.
  define variable lProject like vod_project  no-undo.
  define variable lTaxable as character      no-undo.
  define variable lTax_Usage like pod_tax_usage no-undo.
  define variable lTwoWayPO as logical no-undo. /*SWA*/

  create x-noderef hvoucher.
  create x-noderef hvdetail.
  create x-noderef hnode.
  create x-noderef hnode1.

  lctr  = 0.

  EMPTY TEMP-TABLE ttVoucher NO-ERROR.

  repeat ctr = 1 to hcnode:num-children:
    hcnode:get-child(hvoucher, ctr). /*voucher*/

    assign
      lVend = ""
      lInvoice = "".

    repeat ctr1 = 1 to hvoucher:num-children:
       hvoucher:get-child(hvdetail,ctr1).    /*voucherDetail*/

       if hvdetail:name = "voucherDetail" then do:
          assign
            lctr  = lctr + 1
            lctr1 = 0
            lctr2 = 0
            lTotal    = 0
            lTotal2   = 0
            lVend     = ""
            lEffdate  = ?
            lInvoice  = ""
            lDate     = ?
            lVpopo   = ""
            lvocrterms  = ""  /*SR010421*/
            lvoseparate = no /*SR010421*/
            lTwoWayPO = no. /*SWA*/

          repeat ctr2 = 1 to hvdetail:num-children:
             hvdetail:get-child(hnode,ctr2).

             assign
               lAcct    = ""
               lSub     = ""
               lCc      = ""
               lRcvr    = ""
               lPOLine  = 0
               lProject = ""
               lEntity  = ""
               lDesc    = ""
               lAmt     = 0
               lInvqty  = 0
               lCurramt = 0
               lTaxable = ""
               lTax_Usage = "".

             if hnode:name = "aptotal" then do:
                hnode:get-child(hnode, 1).
                lTotal = decimal(hnode:node-value).
             end.
             if hnode:name = "aptotal2" then do:
                hnode:get-child(hnode, 1).
                lTotal2 = decimal(hnode:node-value).
             end.
             if hnode:name = "apVend" then do:
                hnode:get-child(hnode, 1).
                lVend = hnode:node-value.
             end.
             if hnode:name = "apEffdate" then do:
                hnode:get-child(hnode, 1).
                lEffdate = date(hnode:node-value).
             end.
             if hnode:name = "voInvoice" then do:
                hnode:get-child(hnode, 1).
                lInvoice = hnode:node-value.
             end.
             if hnode:name = "apDate" then do:
                hnode:get-child(hnode, 1).
                lDate = date(hnode:node-value).
             end.
             /*SR010421 - added below*/
             if hnode:name = "voCrTerms" then do:
                hnode:get-child(hnode, 1).
                lvocrterms = hnode:node-value.
             end.
             if hnode:name = "voSeparate" then do:
                hnode:get-child(hnode, 1).
                lvoseparate = logical(hnode:node-value).
             end.
             /*SR010421 - added above*/
                /*****SWA - Added Below******/
             if hnode:name = "voTwoWayPO" then do:
                hnode:get-child(hnode, 1).
                lTwoWayPO = logical(hnode:node-value).
             end.
             if hnode:name = "orderDetail" then do:
                repeat ctr3 = 1 to hnode:num-children:
                   hnode:get-child(hnode1,ctr3).
                   
                   if hnode1:name = "vpoPo" then do:
                      hnode1:get-child(hnode1, 1).
                      lVpopo = hnode1:node-value.
                  end. /*if hnode1:name = "vpoPo"*/
                  
                end. /*repeat ctr3*/
             end. /*else if hnode:name = "orderDetail"*/  
                   /******SWA - Added Above*****/
             if hnode:name = "distributionDetail" then do:
                lctr1 = lctr1 + 1.

                repeat ctr3 = 1 to hnode:num-children:
                   hnode:get-child(hnode1,ctr3).

                   if hnode1:name = "vodAcct" then do:
                      hnode1:get-child(hnode1, 1).
                      lAcct = hnode1:node-value.
                   end.

                   if hnode1:name = "vodSub" then do:
                      hnode1:get-child(hnode1, 1).
                      lSub = hnode1:node-value.
                   end.

                   if hnode1:name = "vodCc" then do:
                      hnode1:get-child(hnode1, 1).
                      lCc = hnode1:node-value.
                   end.

                   if hnode1:name = "vodProject" then do:
                      hnode1:get-child(hnode1, 1).
                      lProject = hnode1:node-value.
                   end.

                   if hnode1:name = "vodEntity" then do:
                      hnode1:get-child(hnode1, 1).
                      lEntity = hnode1:node-value.
                   end.

                   if hnode1:name = "iDesc" then do:
                      hnode1:get-child(hnode1, 1).
                      lDesc = hnode1:node-value.
                   end.

                   if hnode1:name = "vodAmt" then do:
                      hnode1:get-child(hnode1, 1).
                      lAmt = decimal(hnode1:node-value).
                   end.

                   if hnode1:name = "vodTaxable" then do:
                      hnode1:get-child(hnode1, 1).
                      lTaxable = hnode1:node-value.
                   end.

                   if hnode1:name = "taxUsage" then do:
                      hnode1:get-child(hnode1, 1).
                      lTax_Usage = (hnode1:node-value).
                   end.
                end. /* repeat ctr3 */

                FIND FIRST ttVoucher WHERE ttVLine = lctr
                                     EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                IF NOT AVAILABLE ttVoucher THEN DO:
                   create ttVoucher.
                   assign
                      ttTotal   = lTotal
                      ttTotal2  = lTotal2
                      ttVend    = lVend
                      ttInvoice = lInvoice
                      ttDate    = lDate
                      ttVLine   = lctr
                      ttEffdate = lEffdate
                      ttVpopo   = lVpopo when lTwoWayPO = yes  /*SWA*/
                      ttvocrterms = lvocrterms    /*SR010421*/
                      ttvoseparate = lvoseparate /*SR010421*/
                      ttTwowayPO   = lTwoWayPO. /*SWA*/
                END. /*IF NOT AVAILABLE ttVoucher*/
                
                IF lEntity = "" THEN DO:
                   FOR FIRST en_mstr WHERE en_domain = global_domain NO-LOCK:
                   END. /*for first en_mstr*/
                   IF AVAILABLE en_mstr THEN ASSIGN lEntity = en_entity.
                END. /*IF lEntity = "" */

                create ttDetail.
                assign
                   ttLink    = lctr
                   ttAcct    = lAcct
                   ttSub     = lSub
                   ttCc      = lCc
                   ttProject = lProject
                   ttEntity  = lEntity
                   ttDesc    = lDesc
                   ttAmt     = lAmt
                   ttLine    = lctr1
                   ttRLine   = 0
                   ttTaxable = lTaxable
                   ttTax_Usage = lTax_Usage.
             end. /* if hnode:name = "distributionDetail" */
             
            /* **********SWA COMMENTED BELOW************ */
            /* if hnode:name = "orderDetail" then do:       *
             *    repeat ctr3 = 1 to hnode:num-children:    *
             *      hnode:get-child(hnode1,ctr3).           *

             *     if hnode1:name = "vpoPo" then do:        *
             *         hnode1:get-child(hnode1, 1).         *
             *        lVpopo = hnode1:node-value.           *
             *      end.                                    *
             *  end. /*repeat ctr3*/                        *
             * end. /*else if hnode:name = "orderDetail" */ */
              /* *******SWA COMMENTED BELOW************** */
              
             if hnode:name = "receiverMatchingDetail" then do:
                lctr2 = lctr2 + 1.

                repeat ctr3 = 1 to hnode:num-children:
                   hnode:get-child(hnode1,ctr3).

                   if hnode1:name = "receiver" then do:
                      hnode1:get-child(hnode1, 1).
                      lRcvr = hnode1:node-value.
                   end.

                   if hnode1:name = "rcvrLine" then do:
                      hnode1:get-child(hnode1, 1).
                      lPOLine = integer(hnode1:node-value).
                   end.

                   if hnode1:name = "vphInvQty" then do:
                      hnode1:get-child(hnode1, 1).
                      lInvqty = decimal(hnode1:node-value).
                   end.

                   if hnode1:name = "vphCurrAmt" then do:
                      hnode1:get-child(hnode1, 1).
                      lCurramt = decimal(hnode1:node-value).
                   end.

                   if hnode1:name = "taxTaxable" then do:
                      hnode1:get-child(hnode1, 1).
                      lTaxable = hnode1:node-value.
                   end.

                   if hnode1:name = "taxUsage" then do:
                      hnode1:get-child(hnode1, 1).
                      lTax_Usage = (hnode1:node-value).
                   end.
                end. /* repeat ctr3 */

                FIND FIRST ttVoucher WHERE ttVLine = lctr
                                     EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                IF NOT AVAILABLE ttVoucher THEN DO:
                   create ttVoucher.
                   assign
                      ttTotal   = lTotal
                      ttTotal2  = lTotal2
                      ttVend    = lVend
                      ttInvoice = lInvoice
                      ttDate    = lDate
                      ttVLine   = lctr
                      ttEffdate = lEffdate
                      ttVpopo   = lVpopo
                      ttvocrterms = lvocrterms    /*SR010421*/
                      ttvoseparate = lvoseparate /*SR010421*/
                      ttTwowayPO   = lTwoWayPO . /*SWA*/
                END. /*IF NOT AVAILABLE ttVoucher*/

                create ttDetail.
                assign
                   ttLink    = lctr
                   ttRcvr    = lRcvr
                   ttRLine   = lctr2
                   ttInvqty  = lInvqty
                   ttCurramt = lCurramt
                   ttLine    = 0
                   ttPOLine  = lPOLine
                   ttTaxable = lTaxable
                   ttTax_Usage = lTax_Usage.
             end. /* else if hnode:name = "receiverMatchingDetail" */
          end. /* repeat ctr2 */
       end. /* if hvdetail:name = "voucherDetail */
    end. /* repeat ctr1 */
  end. /* repeat ctr */

  delete object hnode1.
  delete object hnode.
  delete object hvdetail.
  delete object hvoucher.
end procedure. /*procedure getReqDetails*/

PROCEDURE procValidate:
   DEFINE VARIABLE m_Vodamt LIKE vod_amt  NO-UNDO.

   FOR EACH ttVoucher EXCLUSIVE-LOCK:
      ASSIGN
         ttHSev = ""
         ttHMsg = "".
      /*check for supplier */
      IF ttVend = "" THEN do:
         ASSIGN
            ttHSev = "error"
            ttHMsg = "Supplier is blank".
            next.
      end. /*IF ttVend = ""*/

      FIND FIRST vd_mstr
          WHERE vd_domain = global_domain
          AND vd_addr     = ttVend NO-LOCK NO-ERROR.
      IF NOT AVAILABLE vd_mstr THEN do:
         ASSIGN
            ttHSev = "error"
            ttHMsg = "Invalid supplier".
         next.
      end. /*IF NOT AVAILABLE vd_mstr*/

      IF vd_ckfrm = '3' OR vd_ckfrm = '4' THEN
      DO:
         find first csbd_det where csbd_domain    = global_domain
                               and csbd_addr      = vd_addr
                               and csbd_beg_date <=  TODAY
                               and csbd_end_date >= TODAY
                             no-lock no-error.
         IF NOT AVAILABLE csbd_det THEN do:
            ASSIGN
               ttHSev = "error"
               ttHMsg = "Supplier Bank does not exist".
            NEXT.
         end. /*IF NOT AVAILABLE csbd_det */

         IF vd_pay_spec = NO THEN do:
            ASSIGN
               ttHSev = "error"
               ttHMsg = "Supplier Pay Specification should be Yes".
            NEXT.
         end. /*IF vd_pay_spec = NO */
      END. /*IF vd_ckfrm = '3' OR vd_ckfrm = '4'*/

      IF ttEffdate = ? THEN do:
         ASSIGN
            ttHSev = "error"
            ttHMsg = "Eff Date cannot be blank".
         next.
      end. /*IF ttEffdate = ?*/

      IF ttInvoice = "" THEN do:
          ASSIGN
             ttHSev = "error"
             ttHMsg = "Invoice cannot be blank".
          next.
      end. /*IF ttInvoice = "" */

      IF ttDate = ? THEN do:
         ASSIGN
            ttHSev = "error"
            ttHMsg = "Invoice date cannot be blank".
         next.
      end. /*IF ttDate = ?*/

      /*SR010421 - added below*/
      IF ttvocrterms <> "" THEN do:
         find ct_mstr where ct_domain = global_domain
                        and ct_code = ttvocrterms
                      no-lock no-error.
         if not available ct_mstr then do:
            ASSIGN
               ttHSev = "error"
               ttHMsg = "Invalid Invoice/Credit Terms".
            next.
         end. /*if not available ct_mstr*/
      end. /*IF ttvocrterms <> ""*/
      /*SR010421 - added above*/

      IF ttTotal = 0 THEN do:
         ASSIGN
            ttHSev = "error"
            ttHMsg = "Control amount cannot be Zero".
         next.
      end. /*IF ttTotal = 0*/

      IF ttTotal2 = 0 THEN do:
         ASSIGN
            ttHSev = "error"
            ttHMsg = "Control amount-2 cannot be Zero".
         next.
      end. /*IF ttTotal2 = 0*/

      /*Check for match Voucher entity and bank entity*/
      FOR FIRST gl_ctrl WHERE gl_domain = global_domain NO-LOCK:
      END. /*FOR FIRST gl_ctrl */

      FIND FIRST vd_mstr WHERE vd_domain = global_domain
                           AND vd_addr   = ttVend
                         NO-LOCK NO-ERROR.
      IF AVAILABLE vd_mstr THEN DO:
         IF vd_bank <> "" THEN DO:
            FIND FIRST bk_mstr WHERE bk_domain = global_domain
                                                AND bk_code   = vd_bank
                               NO-LOCK NO-ERROR.
            IF AVAILABLE bk_mstr AND (AVAILABLE gl_ctrl
                                            AND gl_entity <> bk_entity) THEN DO:
               ASSIGN
                  ttHSev = "error"
                  ttHMsg = "Bank entity must match voucher entity".
               next.
            END. /*IF AVAILABLE bk_mstr AND (AVAILABLE gl_ctrl .....*/
         END. /*IF vd_bank <> ""*/
      END. /*if available vd_mstr*/

      IF ttVpopo <> "" THEN DO:
         FIND FIRST po_mstr WHERE po_domain = global_domain
                              AND po_nbr    = ttVpopo
                            NO-LOCK NO-ERROR.
         IF NOT AVAILABLE po_mstr THEN do:
            ASSIGN
               ttHSev = "error"
               ttHMsg = "Invalid PO Number".
            next.
         end. /*IF NOT AVAILABLE po_mstr */
      END. /*IF ttVpopo <> ""*/

      /****************** Detail table check *****************/

      m_Vodamt = 0.
      FOR EACH ttDetail WHERE ttLink = ttVLine
                        NO-LOCK:

         assign
            m_Vodamt = (m_Vodamt + ttDetail.ttAmt)
                              WHEN ttRcvr = ""
            m_Vodamt = (m_Vodamt + (ttDetail.ttInvqty * ttDetail.ttCurramt))
                                       WHEN ttRcvr <> ""
            m_Vodamt = ROUND(m_Vodamt,2).
      END. /*FOR EACH ttDetail*/

      /*Check to Match Amounts*/
      IF m_Vodamt <> ROUND(ttTotal2,2) THEN DO:
         ASSIGN
            ttHSev = "error"
            ttHMsg = "Control Amount is not equal to sum of Line level Detail "
                     + "Amount".
         next.
      END. /*IF m_Vodamt <> ttTotal2 */

      FOR EACH ttDetail WHERE ttLink = ttVLine
                        EXCLUSIVE-LOCK:
      ASSIGN
         ttSev = ""
         ttMsg = "".

      /*Entity check begins*/
      IF ttEntity <> "" THEN
      DO:
         FIND FIRST en_mstr WHERE en_domain = global_domain
                              AND en_entity = ttEntity
                            NO-LOCK NO-ERROR.
         IF NOT AVAILABLE en_mstr THEN do:
            ASSIGN
               ttSev = "error"
               ttMsg = "Invalid Entity".
            next.
         end. /*IF NOT AVAILABLE en_mstr */

         /*Verify for Open GL period for the Entity*/
         {gpglef01.i ""AP"" en_entity ttEffdate}
         IF gpglef > 0 THEN
         DO:
            ASSIGN
               ttSev = "error"
               ttMsg = "Period has been closed for entity " + en_entity.
            next.
         END. /*IF gpglef > 0*/

         IF gpglef = 0 THEN
         DO:
            ASSIGN m_per_yr = "".
            {glper1.i ttEffdate m_per_yr}
            IF m_per_yr = "" THEN
            DO:
               ASSIGN
                  ttSev = "error"
                  ttMsg = "Invalid Date".
               next.
            END. /*IF m_per_yr = ""*/
         END. /*IF gpglef = 0*/
      END. /*IF ttEntity <> ""*/
      /*Entity check ends*/

      IF (ttTaxable <> "Yes"
      AND ttTaxable <> "No") THEN
      DO:
         ASSIGN
            ttSev = "error"
            ttMsg = "Taxable Flag should either be Yes or No".
         NEXT.
      END. /*IF (ttTaxable <> "Yes" and ...)*/

      IF ttTax_Usage <> "" THEN DO:
         FIND FIRST code_mstr WHERE code_domain  = global_domain
                                AND code_fldname = "tx2_tax_usage"
                                AND code_value   = ttTax_Usage
                              NO-LOCK NO-ERROR.
         IF NOT AVAILABLE code_mstr THEN DO:
            ASSIGN
               ttSev = "error"
               ttMsg = "Tax usage does not exist".
            NEXT.
         END. /*IF NOT AVAILABLE code_mstr */
      END. /*IF ttTax_Usage <> "" */
      
      /**************SWA ADDED BELOW*************/
      IF ttTwowaypo = YES THEN DO:
      
         IF ttVpopo = "" THEN DO: 
            ASSIGN
               ttSev = "error"
               ttMsg = "Po Number must exist for two way PO".
            NEXT.
         END. /*if ttVpopo = ""*/
         
         ELSE IF ttVpopo <> "" then  DO:
            FIND FIRST po_mstr WHERE po_domain = global_domain
                                AND po_nbr    = ttVpopo
                                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE po_mstr THEN DO:
               ASSIGN
                  ttSev = "error"
                  ttMsg = "Invalid PO Number".
               NEXT.
            END. /*IF NOT AVAILABLE po_mstr */
         END. /*else ttVpopo <> "" */
         IF ttRcvr <> "" THEN DO:
            ASSIGN
               ttSev = "error"
               ttMsg = "Receiver is not applicable for two way PO".
            NEXT.
         END. /*ttRcvr < > ""*/
      END. /*if ttTwowaypo = yes */
      /***********SWA ADDED ABOVE*****************/
      IF ttRcvr = "" THEN DO:
         IF ttAcct = "" THEN do:
            ASSIGN
               ttSev = "error"
               ttMsg = "Account Code cannot be blank".
            next.
         end. /*IF ttAcct = ""*/

         /* Account Master Validation */
         FIND FIRST ac_mstr WHERE ac_domain = global_domain
                              AND ac_code   = ttAcct
                            NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ac_mstr THEN
         DO:
            ASSIGN
               ttSev = "error"
               ttMsg = "Inactive Account Code".
            next.
         END. /*IF NOT AVAILABLE ac_mstr */
         IF NOT ac_active THEN do:
            ASSIGN
               ttSev = "error"
               ttMsg = "Inactive Account Code".
            next.
         end. /*IF NOT ac_active */

         IF ttSub <> "" THEN DO:
            FIND FIRST sb_mstr WHERE sb_domain = global_domain
                                 AND sb_sub    = ttSub
                               NO-LOCK NO-ERROR.
            IF NOT AVAILABLE sb_mstr THEN
            DO:
               ASSIGN
                  ttSev = "error"
                  ttMsg = "Inactive Sub Account Code".
               next.
            END. /*IF NOT AVAILABLE sb_mstr */
            IF NOT sb_active THEN do:
               ASSIGN
                  ttSev = "error"
                  ttMsg = "Inactive Sub Account Code".
               next.
            end. /*IF NOT sb_active */
         END. /*IF ttSub <> "" */

         IF ttCc <> "" THEN DO:
            FIND FIRST cc_mstr WHERE cc_domain = global_domain
                                 AND cc_ctr    = ttCc
                               NO-LOCK NO-ERROR.
            IF NOT AVAILABLE cc_mstr THEN
            DO:
               ASSIGN
                  ttSev = "error"
                  ttMsg = "Inactive Cost Center Code".
               next.
            END. /*IF NOT AVAILABLE cc_mstr */
            IF NOT cc_active THEN do:
               ASSIGN
                  ttSev = "error"
                  ttMsg = "Inactive Cost Center Code".
               next.
            end. /*IF NOT cc_active*/
         END. /*IF ttCc <> "" */

         IF ttProject <> "" THEN DO:
            FIND FIRST pj_mstr WHERE pj_domain  = global_domain
                                 AND pj_project = ttProject
                               NO-LOCK NO-ERROR.
            IF NOT AVAILABLE pj_mstr THEN
            DO:
               ASSIGN
                  ttSev = "error"
                  ttMsg = "Inactive Project Code".
               next.
            END. /*IF NOT AVAILABLE pj_mstr */
            IF NOT pj_active THEN do:
               ASSIGN
                  ttSev = "error"
                  ttMsg = "Inactive Project Code".
               next.
            end. /*IF NOT pj_active */
         END. /*IF ttProject <> "" */

         /*Validations for Dist Amount*/
         IF ttAmt = 0.0 THEN
         DO:
            ASSIGN
               ttSev = "error"
               ttMsg = "Distribution Line Amount cannot be 0".
            next.
         END. /*IF tt_amt = 0.0*/
         IF (ttAmt < -9999999999.99 OR
            ttAmt > 9999999999.99) THEN do:
            ASSIGN
               ttSev = "error"
               ttMsg = "Max field length exceeded for Dist Amt".
            next.
         end. /*IF (ttAmt < -9999999999.99 OR ....*/
      END. /*IF ttRcvr = ""*/

      ELSE DO:
         IF ttPOLine = 0 THEN do:
            ASSIGN
               ttSev = "error"
               ttMsg = "PO Line number cannot be blank".
            next.
         end. /*IF ttPOLine = 0 */

         FIND FIRST pod_det WHERE pod_domain = global_domain
                              AND pod_nbr    = ttVpopo
                              AND pod_line   = ttPOLine
                            NO-LOCK NO-ERROR.
         IF NOT AVAILABLE pod_det THEN do:
            ASSIGN
               ttSev = "error"
               ttMsg = "PO Line number does not exist".
            next.
         end. /*IF NOT AVAILABLE pod_det*/

         IF ttInvqty = 0 THEN do:
            ASSIGN
               ttSev = "error"
               ttMsg = "Invoice Quantity cannot be zero".
            next.
         end. /*IF ttInvqty = 0*/

         IF ttCurramt = 0 THEN do:
            ASSIGN
               ttSev = "error"
               ttMsg = "Invoice Cost cannot be zero".
            next.
         end. /*IF ttCurramt = 0*/

         /*Invalid Receiver check*/
         find first prh_hist
            where prh_domain   = global_domain
              and prh_receiver = ttRcvr
              and prh_line     = ttPOLine
              and can-find (first pvo_mstr
                     where pvo_mstr.pvo_domain  = global_domain and
                     pvo_lc_charge              = ""
                      and pvo_internal_ref_type = "07"
                      and pvo_internal_ref      = ttRcvr
                      and pvo_line              = ttPOLine
                      and pvo_last_voucher      = "")
              no-lock no-error.
         if not available prh_hist then do:
            ASSIGN
               ttSev = "error"
               ttMsg = "Invalid receiver".
            next.
         end. /*if not availabale prh_hist*/
      END. /*else IF ttRcvr <> "" */

      END. /*for each ttDetail*/
   END. /*for each ttVoucher*/
END PROCEDURE. /*procValidate*/

PROCEDURE procCim:
   DEFINE VARIABLE m_space    LIKE mfc_char FORMAT "x(1)" INITIAL " "  NO-UNDO.
   DEFINE VARIABLE m_quote    LIKE mfc_char FORMAT "x(4)" INITIAL """" NO-UNDO.
   DEFINE VARIABLE m_outfile    AS CHARACTER  FORMAT "x(50)"           NO-UNDO.
   DEFINE VARIABLE m_erroutfile AS CHARACTER  FORMAT "x(50)"           NO-UNDO.
   DEFINE VARIABLE m_cimfile    AS CHARACTER  FORMAT "x(50)"           NO-UNDO.
   DEFINE VARIABLE m_errcimfile AS CHARACTER  FORMAT "x(50)"           NO-UNDO.
   DEFINE VARIABLE m_errline    AS CHARACTER  FORMAT "x(65)"           NO-UNDO.
   DEFINE VARIABLE trecid       AS RECID                               NO-UNDO.
   DEFINE VARIABLE prev_vouchered_qty LIKE pvo_vouchered_qty           NO-UNDO.

   ASSIGN
      m_outfile = "out" + STRING(DAY(TODAY)) + STRING(MONTH(TODAY))
                   + STRING(REPLACE(STRING(TIME,"HH:MM"),":","")) + ".out"
      m_cimfile = "cim" + STRING(DAY(TODAY)) + STRING(MONTH(TODAY))
                + STRING(REPLACE(STRING(TIME,"HH:MM"),":","")) + ".cim"
      m_errline = "".

   FOR EACH ttVoucher WHERE ttHSev <> "error"
                      EXCLUSIVE-LOCK
                      BREAK BY ttVline:
      trecid = ?.
      FIND FIRST ttDetail
           WHERE ttDetail.ttLink = ttVLine
             AND ttSev = "error"
           NO-LOCK NO-ERROR.
      IF AVAILABLE ttDetail THEN DO:
         ASSIGN
            ttVoucher.ttHSev = "error"
            trecid           = recid(ttDetail).
         FOR EACH ttDetail WHERE ttDetail.ttLink = ttVLine
                             AND recid(ttDetail) <> trecid
                             AND ttDetail.ttSev  <> "error"
                           EXCLUSIVE-LOCK:
            ASSIGN
               ttDetail.ttSev = "error"
               ttDetail.ttMsg = "Invoice not processed as error in other line".
         END. /* FOR EACH ttDetail */
         NEXT.
      END. /*IF AVAILABLE ttDetail */
      OUTPUT TO VALUE(m_cimfile).

      PUT UNFORMATTED
         "-"  m_space  SKIP.                               /*Batch No*/
      PUT UNFORMATTED
         "-"  m_space  SKIP.                               /*Control amt*/
      PUT UNFORMATTED
         "-"  m_space  SKIP.                               /*Voucher ref*/

      IF ttVpopo <> "" THEN
         PUT UNFORMATTED
            m_quote ttVpopo m_quote m_space  SKIP          /*Purchase Order*/
            "." m_space                      SKIP.
      ELSE PUT UNFORMATTED "." SKIP.

      PUT UNFORMATTED
         "-" m_space /*m_quote ttTotal m_quote m_space */    /*Aptotal*/
         m_quote REPLACE(ttVend,'"','""')  m_quote m_space   /*Supplier Code*/
         m_quote ttEffdate m_quote m_space         SKIP.     /*Effective Date*/
      PUT UNFORMATTED
         "-" m_space                                         /*Currency*/
         "-" m_space                                         /*Bank*/
         m_quote REPLACE(ttInvoice,'"','""') m_quote m_space /*Invoice*/
         m_quote ttDate m_quote m_space                      /*AP Date*/
  /*"-" m_space                                         /*Terms*/*/ /*SR010421*/
    m_quote REPLACE(ttvocrterms,'"','""') m_quote m_space /*Terms*/ /*SR010421*/
         "-" m_space                                         /*Disc Date*/
         "-" m_space                                         /*Due Date*/
         "-" m_space                                         /*Expected*/
         "-" m_space                                         /*Account*/
         "-" m_space                                         /*Sub Account*/
         "-" m_space                                         /*Cost Centre*/
         "-" m_space                                         /*Disc Account*/
         "-" m_space                                         /*Disc SubAccount*/
         "-" m_space                                         /*Disc CostCentre*/
         "-" m_space                                         /*Entity*/
         "-" m_space                                         /*Remark*/
         "-" m_space                                         /*Supplier Bank*/
/*"-" m_space                                     /*Separate Ck*/*/ /*SR010421*/
m_quote ttvoseparate m_quote m_space                /*Separate Ck*/ /*SR010421*/
         m_quote "0" m_quote m_space SKIP.                   /*Voucher Type*/

      PUT UNFORMATTED "-" m_space SKIP.            /*Prepay Amt hold Frame*/
      IF ttVpopo <> "" THEN
         PUT UNFORMATTED "-" m_space SKIP.         /*Auto Select Field*/
      PUT UNFORMATTED "-" m_space SKIP.            /*Tax Detail frame*/

      FOR EACH ttDetail
         WHERE ttDetail.ttLink = ttVLine
           AND ttSev <> "error"
           EXCLUSIVE-LOCK BREAK BY ttLine:

         IF ttRcvr = "" THEN DO:
           /*SWA ADDED BELOW */
            if ttTwoWaypo = yes then   /*Skip Receiver frame for TwowayPO*/
               put unformatted "." skip .  
           /*SWA ADDED ABOVE*/
            IF ttVpopo = "" THEN 
               PUT UNFORMATTED
               m_quote ttLine m_quote m_space        SKIP.  /*Voucher Line*/
            ELSE
            PUT UNFORMATTED
               "-" m_space                           SKIP. /*Voucher Line*/
            PUT UNFORMATTED
               m_quote REPLACE(ttAcct ,'"','""') m_quote m_space /*Account*/
               m_quote REPLACE(ttSub  ,'"','""') m_quote m_space /*Sub-Acct*/
               m_quote REPLACE(ttCc ,'"','""') m_quote m_space   /*CC*/
               m_quote REPLACE(ttProject ,'"','""') m_quote m_space
                                                               /*Project*/
               m_quote REPLACE(ttEntity ,'"','""') m_quote m_space. /*Entity*/
            PUT UNFORMATTED
               m_quote ttTaxable m_quote m_space SKIP.  /*Tax*/

            /*Tax detail frame*/
            IF ttTaxable = "Yes" THEN DO:
            PUT UNFORMATTED
               m_quote ttTax_Usage m_quote m_space             /*Tax Usage*/
               "-" m_space                                     /*Tax Class*/
               m_quote ttTaxable m_quote m_space SKIP.         /*Taxable*/
            END. /*IF ttTaxable = "Yes"*/

            PUT UNFORMATTED
               m_quote ttDesc m_quote m_space      SKIP.  /*Description*/
            PUT UNFORMATTED
               m_quote ttAmt m_quote m_space       SKIP.  /*Amount*/
         END. /*IF ttRcvr = "" */

         ELSE IF ttRcvr <> "" THEN DO:
            PUT UNFORMATTED
               m_quote ttRcvr   m_quote m_space                /*Receiver*/
               m_quote ttPOLine m_quote m_space SKIP.          /*PO Line*/

            IF ttTax_Usage = "" THEN
            PUT UNFORMATTED "-" m_space SKIP.            /*Tax detail frame*/

            ELSE     
            /*Tax detail frame*/
            PUT UNFORMATTED
               m_quote ttTax_Usage m_quote m_space             /*Tax Usage*/
               "-" m_space                                     /*Tax Class*/
               m_quote ttTaxable m_quote m_space SKIP.         /*Taxable*/

            FIND FIRST prh_hist WHERE prh_domain   = global_domain
                                  AND prh_nbr      = ttVpopo
                                  AND prh_receiver = ttRcvr
                                  AND prh_line     = ttPOLine
                                NO-LOCK NO-ERROR.
            PUT UNFORMATTED
               m_quote ttInvqty  m_quote m_space       /*Invoice Qty*/
               m_quote ttCurramt m_quote m_space.      /*Invoice Cost*/

            /*Check for Memo Item*/
            FIND FIRST pt_mstr WHERE pt_domain = global_domain
                                 AND pt_part   = prh_part
                               NO-LOCK NO-ERROR.
            IF NOT AVAILABLE pt_mstr THEN
            PUT UNFORMATTED
               "-" m_space SKIP.                  /*Acc/Sub Acc/CC/Project*/

            ELSE PUT UNFORMATTED m_space SKIP.

            prev_vouchered_qty = 0.
            /* Check Partial Invoicing for Standard & Memo Items - Line close*/
            FOR EACH pvo_mstr
                WHERE pvo_mstr.pvo_domain  = global_domain
                 AND pvo_lc_charge         = ""
                 AND pvo_internal_ref_type = "07"
                 AND pvo_internal_ref      = ttRcvr
                 AND pvo_line              = ttPOLine
               NO-LOCK:
               ASSIGN
                  prev_vouchered_qty = prev_vouchered_qty + pvo_vouchered_qty.
            END. /*for each pvo_mstr */

            IF (AVAILABLE prh_hist AND
               (prev_vouchered_qty + ttInvqty) <> prh_rcvd) THEN
               PUT UNFORMATTED "-" m_space SKIP.     /*Close Line*/

            IF LAST-OF(ttLine) THEN DO:
               PUT UNFORMATTED "." m_space          SKIP. /*Receiver detail*/
               PUT UNFORMATTED "." m_space          SKIP. /*scroll frame*/
            END. /*IF LAST-OF(ttVline)*/
         END. /*else IF ttRcvr <> "" */

         IF LAST(ttLine) THEN DO:
            PUT UNFORMATTED
               "." m_space                  SKIP. /*Last Distribution line*/
            PUT UNFORMATTED
               "-" m_space                  SKIP. /*View/Edit tax detail*/
            PUT UNFORMATTED
               "-" m_space                  SKIP. /*Hold Amount Frame*/
            PUT UNFORMATTED
               "." m_space                  SKIP. /*Voucher Number*/
            PUT UNFORMATTED
               "." m_space                  SKIP. /*Batch Number*/
         END. /*IF LAST(ttLine)*/
      END. /*FOR EACH ttDetail*/
      OUTPUT CLOSE. /*OUTPUT TO VALUE(m_cimfile)*/

      cimloop:
      DO TRANSACTION ON ERROR UNDO cimloop, LEAVE cimloop:
         INPUT FROM VALUE(m_cimfile).
         OUTPUT TO VALUE(m_outfile).
            BATCHRUN = YES.
            /* Voucher maintenance program */
            {gprun.i ""apvomt.p""}
            BATCHRUN = NO.
         OUTPUT CLOSE. /*OUTPUT TO   VALUE(m_outfile) */
         INPUT  CLOSE. /*INPUT  FROM VALUE(m_cimfile) */

         ASSIGN
            m_errline = "".

         INPUT FROM VALUE(m_outfile).
            REPEAT:
               ASSIGN m_errline = "".
               IMPORT UNFORMATTED m_errline.

               IF INDEX(m_errline,"ERROR") <> 0 OR
                  m_errline begins "** " OR
                  INDEX(m_errline,"WARNING") <> 0 THEN
               DO:
                  ASSIGN
                     ttHSev = "error" WHEN (INDEX(m_errline,"ERROR") <> 0 OR
                                            m_errline begins "** ")
                     ttHSev = "warning" WHEN INDEX(m_errline,"WARNING") <> 0
                     ttHMsg = m_errline.
               END. /*IF INDEX(errline,"ERROR") <> 0*/

               IF INDEX(m_errline,"Batch:") <> 0 and ttBatch = "" THEN
                  ttBatch = trim(substring(m_errline,10,8)).

               IF INDEX(m_errline,"Ref:") <> 0 and ttApRef = "" THEN
                  ttApRef = trim(substring(m_errline,12,9)).
            END. /*REPEAT */
         INPUT CLOSE. /*INPUT FROM VALUE(m_outfile)*/

         IF ttHSev = "error" THEN DO:
            ASSIGN
               m_errcimfile = m_cimfile + "-" + string(ttVLine)
               m_erroutfile =   m_outfile + "-" + string(ttVLine).

            OS-COPY VALUE(m_cimfile) VALUE(m_errcimfile).
            OS-COPY VALUE(m_outfile) VALUE(m_erroutfile).
            UNDO cimloop, LEAVE cimloop.
         END. /*IF ttHSev = "error" */

         IF ttHSev <> "error" THEN DO:
            for last ap_mstr where ap_domain = global_domain
                               and ap_type   = "VO"
                               and ap_ref    = ttApRef
                               no-lock:
            end. /*for last ap_mstr */
            if available ap_mstr then do:
               if (ap_amt <> ttTotal) then
               assign
                  ttHSev = ""
                  ttHMsg = "Info: QAD AP total with tax"
                           + " calculation does not match the invoice".
            end. /* if available ap_mstr*/
         END. /*IF ttHSev <> "error" AND ttHmsg <> "" */
      END. /*cimloop*/
   END. /*FOR EACH ttVoucher */

   /*Delete the CIM file & Outfile*/
   OS-DELETE VALUE(m_cimfile).
   OS-DELETE VALUE(m_outfile).   
END PROCEDURE. /*procCim*/

procedure procErrorReport:
  define input parameter inpErrFile as character no-undo.

  output stream strmErrRpt to value(inpErrFile).
    put stream strmErrRpt unformatted
       'Voucher#|'
       'QAD Batch|'
       'QAD Voucher|'
       'Supplier|'
       'Invoice|'
       'Line#|'
       'Account|'
       'Sub-Acct|'
       'Cost-Center|'
       'Receiver|'
       'PO Line|'
       'Severity|'
       'Message' skip.

    for each ttVoucher no-lock,
       each ttDetail where ttLink = ttVLine
                     no-lock break by ttVline:

       put stream strmErrRpt unformatted
          ttVLine "|"
          ttBatch "|"
          ttApRef "|"
          ttVend "|"
          ttInvoice "|"
          ttLine "|"
          ttAcct "|"
          ttSub "|"
          ttCc "|"
          ttRcvr "|"
          ttPOLine "|"
          ttHSev "|"
          (ttHMsg + ttMsg) skip.
    end. /* for each ttVoucher */
  output stream strmErrRpt close.
end procedure. /*procedure procErrorReport*/
