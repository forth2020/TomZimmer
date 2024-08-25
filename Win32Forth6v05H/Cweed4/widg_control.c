/* ************************************************************************  */
/*                                                                           */
/* ========================================================================= */
/*                                                                           */
/*                                                                           */
/* ========================================================================= */
/*                                                                           */
/*  File Name : widg_control.c                                               */
/*                                                                           */
/*  Purpose   : Universal Widget Application Controller                      */
/*                                                                           */
/*  Warnings  :                                                              */
/*                                                                           */
/*****************************************************************************/
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

/************************  INCLUDE FILES   ***********************************/

#include <string.h>
#include <stdlib.h>

#include "mmi_types.h"  /*t_ucs2_string*/
#include "our_core.h"
#include "widj_public.h"
#include "widg_private.h"
#include "gui_types.h"  /*t_font_style*/
#include "mmi_module.h" /*module_ids*/
#include "aman_public.h"
#include "srtd_public.h"
#include "adinft_public.h"
#include "agsky_public.h"
#include "aglst_public.h"
#include "agpop_public.h"
#include "agedit_public.h"
#include "agmenu_public.h" /*for menu fncts */
#include "slib_public.h" /* for widgSortList fnc. and SlibString functions*/
#include "alib_public.h" /* for AlibAcknowledgeKey */

/********************** LOCAL CONSTANT DEFINITIONS  **************************/

/*****************************************************************************/
/*                    LOCAL ENUM/TYPE DEFINITIONS                            */
/*****************************************************************************/

/*****************************************************************************/
/*                    LOCAL VARIABLE DEFINITIONS                             */
/*****************************************************************************/

t_widg_status           widg_status;
t_widj_play_options     play_opts;
t_byte                  current_language;
t_font_style            text_font;
t_font_style            info_hdr_font;
t_font_style            info_bdy_font;
e_aglst_align           list_alignment;
t_widg_list_info        list_info;
t_widg_list_info        mnglist_info;
e_widg_status           widgmanage_result;
e_widg_status           widglist_result;

t_widj_list_entry     * widjlist_ptr=NULL_PTR;
t_widg_list_info      * widglist_ptr=NULL_PTR;

t_agedit_alpha_handle   save_alpha_handle;
t_agedit_alpha_handle   rename_alpha_handle;

/*****************************************************************************/
/*                    LOCAL FUNCTION PROTOTYPES                              */
/*****************************************************************************/

LOCAL_FUNC t_void widgStateHandler (t_bool,
                                    e_widg_states,
                                    t_bool,
                                    e_widg_subapp_substates);

LOCAL_FUNC t_void widgSetPlayOptions (t_void);

LOCAL_FUNC t_void widgGetListAlignment (t_void);

LOCAL_FUNC t_void widgGetStyles(t_void);

LOCAL_FUNC t_void widgSetGlobals (t_void);

LOCAL_FUNC int widgAlphaCompare (const t_void *,
                                 const t_void *);

LOCAL_FUNC t_void widgSortList (t_widj_list_entry *,
                                t_word);

LOCAL_FUNC t_word widgGetListIndex(t_widj_media_id,
                                   t_widj_list_entry*,
                                   t_word);

LOCAL_FUNC e_widj_status widgGetwidjListEntries (t_widg_list_info *);

LOCAL_FUNC t_void widgSetPickListInfo (t_widg_list_info*);

LOCAL_FUNC t_void widgCreatePickList(t_widg_list_info *);

LOCAL_FUNC t_void widgSaveMedia (t_widg_save*);

LOCAL_FUNC e_widg_status widgSetwidgResult (e_widj_status);

LOCAL_FUNC t_void widgSelectReturn (e_widg_status);

LOCAL_FUNC t_void widgSaveReturn (e_widg_status);

LOCAL_FUNC t_void widgStoreReturn (e_widg_status);

LOCAL_FUNC t_void widgResetListParams (t_widg_list_info*,
                                       t_mmi_module_id);

LOCAL_FUNC t_void widgSuspendList (t_widg_list_info*,
                                   e_widg_subapp_substates,
                                   t_mmi_module_id);

LOCAL_FUNC t_void widgResumeList (t_widg_list_info*,
                                  e_widg_subapp_substates,
                                  t_mmi_module_id);

LOCAL_FUNC t_void widgCreateManageList (e_slpk_text_id);

LOCAL_FUNC t_void widgManageReturn (e_widg_status);

LOCAL_FUNC t_void widgStopListHandler(t_widg_list_info*,
                                      t_mmi_module_id);

LOCAL_FUNC t_void widgPlayAudio (t_widg_list_info*,
                                 t_mmi_module_id);

LOCAL_FUNC t_void widgViewMedia (t_widg_list_info*,
                                 t_mmi_module_id);

LOCAL_FUNC t_void widgProcessGuiListResults (t_mmi_event_id,
                                             t_widg_list_info*,
                                             t_mmi_module_id,
                                             e_widg_status*);

LOCAL_FUNC t_void widgListEventHandler (t_mmi_event_id,
                                        t_widg_list_info *,
                                        e_widg_status * ,
                                        t_mmi_module_id);

LOCAL_FUNC t_void widgWaitEventHandler (t_mmi_event_id,
                                        t_widg_list_info *,
                                        e_widg_status * ,
                                        t_mmi_module_id);

LOCAL_FUNC t_void widgInfoEventHandler (t_widg_list_info*,
                                        t_mmi_event_id,
                                        t_mmi_module_id);

LOCAL_FUNC t_void widgStopSubappEventHandler(t_mmi_event_id,
                                             t_mmi_module_id);

/* **************************************************************************
 *  widgStateHandler ()
 * **************************************************************************
 * Description:
 *      This function gets the new_state information for the widg module
 *      and sets the value of widg state in widg_status structure.
 * Parameters:
 *      e_widg_states
 * Globals:
 *
 * Return Values:
 *      None.
 * Exceptions:
 *
 * Warnings:
 *
 */

LOCAL_FUNC t_void widgStateHandler (t_bool change_state,
                         e_widg_states new_state,
                         t_bool change_subapp_substate,
                         e_widg_subapp_substates subapp_substate )
{
    if(change_state==TRUE)
    {
       switch (new_state)
        {
          case INIT_STATE:
          case LIST_STATE:
          case PREVIEW_STATE:
          case MANAGER_STATE:
            widg_status.widg_state = new_state;
            break;
          default:
          break;
        }
    }

  /*  if(change_subapp_state==TRUE)
   {
      switch (subapp_state)
      {
        case STOPPED:
        case WAIT_FOR_START:
        case RUNNING:
        case SUSPENDED:
        case WAIT_FOR_TIMER:
       }
   } */

    if(change_subapp_substate==TRUE)
    {
       switch (subapp_substate)
        {
          case widg_SELECT_LIST:
          case widg_SELECT_RADIO_LIST:
          case widg_SELECT_WAIT_CONV_PREVIEW:
          case widg_SELECT_WAIT_CONV_RETURN:
          case widg_SELECT_PREV_UNAVAILABLE:
          case widg_SELECT_PREV_UNAV_RETURN:
          case widg_SELECT_NO_OBJECTS_FOUND:
          case widg_STORE_LIST:
          case widg_STORE_ALREADY_EXISTS:
          case widg_STORE_SAVED_CNF:
          case widg_STORE_READONLY:
          case widg_STORE_PREV_UNAVAILABLE:
          case widg_STORE_WAIT_CONV_PREVIEW:
          case widg_STORE_SAVE_AS_EDIT:
          case widg_STORE_FREE_STORE_QUERY:
          case widg_STORE_SAVE_FAILED:
          case widg_SAVE_MEDIA:
          case widg_SAVE_SAVE_FAILED:
          case widg_SAVE_READONLY:
          case widg_SAVE_SAVED_CNF:
          case widg_SAVE_FREE_STORE_QUERY:
             widg_status.list_substate = subapp_substate;
             break;
          case widg_MANAGER_LIST:
          case widg_MANAGER_DELETE_QUERY:
          case widg_MANAGER_DELETEALL_QUERY:
          case widg_MANAGER_RENAME_EDIT:
          case widg_MANAGER_RENAMED_TO:
          case widg_MANAGER_RENAME_FAILED:
          case widg_MANAGER_RENAME_ALREADY_EXISTS:
          case widg_MANAGER_WAIT_CONV_PREVIEW:
          case widg_MANAGER_PREV_UNAVAILABLE:
          case widg_MANAGER_DELETED_CNF:
          case widg_MANAGER_DELETE_FAILED:
          case widg_MANAGER_DELETE_READONLY:
          case widg_MANAGER_ALL_DELETED_CNF:
          case widg_MANAGER_SOME_DELETED_CNF:
     /*     case widg_MANAGER_WAIT_DELETE_ALL:*/
             widg_status.manager_substate = subapp_substate;
             break;
          case widg_PREVIEW_ACTIVE:
          case widg_PREVIEW_PREV_UNAVAILABLE:
             widg_status.preview_substate = subapp_substate;
             break;
          case widg_LIST_IDLE:
          case widg_MANAGER_IDLE:
          case widg_PREVIEW_IDLE:
           break;

        default:
          break;
        }
    }
}

/* ***************************************************************************
 *  widgSetPlayOptions ()
 * ***************************************************************************
 * Description:
 *      This function sets the play options.
 *
 * Parameters:
 *      None.
 * Globals:
 *      play_opts
 * Return Values:
 *      None.
 * Exceptions:
 *      None.
 * Warnings:
 *      None.
 */

LOCAL_FUNC t_void widgSetPlayOptions (t_void)

{
    /* animation options */
    play_opts.play_settings = widj_PLAY_ONCE;
    play_opts.max_play_time = 0 ; /*t_long, will not be used for image*/
    play_opts.loop_delay_time = 0 ; /*t_long*/
/* audio options */
/*    play_opts.tone_group_id = SAUD_GROUP_RING_TONES; */
 /*SAUD_GROUP_SMS_RING_TONES,SAUD_GROUP_EMS_TONES*/

/* image options */
    play_opts.coords.x = 0; /*t_sword*/
    play_opts.coords.y = 0; /**/
    play_opts.position = CENTRE; /*e_widj_position*/
}

/* ***************************************************************************
 *  widgSetListState ()
 * ***************************************************************************
 * Description:
 *      This function sets the subapp`s substates according to the list_type.
 * Parameters:
 *      t_mmi_module_id , t_bool
 * Globals:
 *      t_widg_list_info, t_widg_status
 * Return Values:
 *      None.
 * Exceptions:
 *      None.
 * Warnings:
 *      None.
 */

GLOBAL_FUNC t_void widgSetListState (t_mmi_module_id mod_id, t_bool preview_state)

{
  if(mod_id==widg_LIST_MODID && preview_state ==FALSE)
   {
     switch(list_info.list_type)
     {
       case SELECT_RADIO_BTN_LIST:
         widg_status.list_substate = widg_SELECT_RADIO_LIST;
         break;
       case SELECT_LIST:
         widg_status.list_substate = widg_SELECT_LIST;
         break;
       case STORE_LIST:
         widg_status.list_substate = widg_STORE_LIST;
         break;
       default:
         break;
     }
   }

  if(mod_id==widg_LIST_MODID && preview_state ==TRUE)
   {
     switch(list_info.list_type)
     {
       case SELECT_RADIO_BTN_LIST:
       case SELECT_LIST:
         widg_status.list_substate = widg_SELECT_WAIT_CONV_PREVIEW;
         break;
       case STORE_LIST:
         widg_status.list_substate = widg_STORE_WAIT_CONV_PREVIEW;
         break;
       default:
         break;
     }
   }

  if(mod_id==widg_MANAGER_MODID && preview_state ==FALSE)
   {
    widg_status.manager_substate = widg_MANAGER_LIST;
   }

  if(mod_id==widg_MANAGER_MODID && preview_state ==TRUE)
   {
    widg_status.manager_substate = widg_MANAGER_WAIT_CONV_PREVIEW;
   }
}

/* ***************************************************************************
 *  widgGetListAlignment ()
 * ***************************************************************************
 * Description:
 *      This function gets the alignment according to the current language.
 *
 * Parameters:
 *      None.
 * Globals:
 *      e_aglst_align list_alignment
 * Return Values:
 *      None.
 * Exceptions:
 *      None.
 * Warnings:
 *      None.
 */

LOCAL_FUNC t_void widgGetListAlignment (t_void)

{
    t_byte           current_language;

/*    current_language = SrtdReadByte(SRTD_LANGUAGE); */

/*check language ranges and assign a default value if necessary*/

/*    list_alignment   = SlpkGetLanguageAlignment(current_language);*/

    list_alignment = AGLST_ALIGN_LEFT;
}

/* ***************************************************************************
 *  widgGetStyles ()
 * ***************************************************************************
 * Description:
 *      This function gets the styles to be used for infotext and text editor
 *
 * Parameters:
 *      None.
 * Globals:
 *      list_alignment
 * Return Values:
 *      None.
 * Exceptions:
 *      None.
 * Warnings:
 *      None.
 */

LOCAL_FUNC t_void widgGetStyles (t_void)

{
    text_font      = gui_text_styles[GUI_DEFAULT_TEXT_STYLE];
    info_hdr_font  = gui_text_styles[GUI_INFOTEXT_HEADER_TEXT_STYLE];
    info_bdy_font  = gui_text_styles[GUI_INFOTEXT_BODY_TEXT_STYLE];
   /*text editor regions ?*/
}

/* ***************************************************************************
 *  widgSetGlobals ()
 * ***************************************************************************
 * Description:
 *      This function sets the globals of widg.
 *
 * Parameters:
 *      None.
 * Globals:
 *      e_aglst_align, t_font_style
 * Return Values:
 *      None.
 * Exceptions:
 *      None.
 * Warnings:
 *      None.
 */

LOCAL_FUNC t_void widgSetGlobals (t_void)

{
    widgSetPlayOptions();
    widgGetListAlignment();
    widgGetStyles();
    /*text editor regions ?*/
}

/************************************************************************
*   widgAlphaCompare()
***********************************************************************
*
* Description:
*    A comparison function for qsort. Sorts a t_widj_list_entry
*    array alphabetically on media_name.
*
* Parameters:
*    elem1, elem2 are pointers to two elements of the array to
*                 compare.
*
* Globals:
*    None
*
* Return value:
*    Returns -1 if elem1 should be before elem2
*    Returns +1 if elem1 should be after  elem2
*    Returns  0 if elem1 and elem2 are the same
*
* Exceptions:
*       None
*
* Warnings:
*       This function has to return int type for qsort function.
*/

LOCAL_FUNC int widgAlphaCompare (const t_void *elem1,
                                 const t_void *elem2)
{
    t_widj_list_entry *elem1_ptr = (t_widj_list_entry *) elem1;
    t_widj_list_entry *elem2_ptr = (t_widj_list_entry *) elem2;
    t_ucs2_string      string1;
    t_ucs2_string      string2;
    t_word             max_length;

    string1.start  = elem1_ptr->media_name;
    string1.length = elem1_ptr->media_name_length;

    string2.start  = elem2_ptr->media_name;
    string2.length = elem2_ptr->media_name_length;

    if (string1.length >= string2.length)
    {
        max_length = string1.length;
    }
    else
    {
        max_length = string2.length;
    }

    return (int) SlibStringCmp(string1, string2, max_length);
}

/************************************************************************
*    widgSortList()
***********************************************************************
*
* Description:
*    Sorts an array of t_widj_list_entry structures alphabetically
*    by media_name (see structure definition).
*
* Parameters:
*    list_ptr     Pointer to the array of t_widj_list_entry.
*    num_entries  Number of elements in the array.
*
* Globals:
*    None
*
* Return value:
*    None
*
* Exceptions:
*    None
*
* Warnings:
*    None
*/
LOCAL_FUNC t_void widgSortList(t_widj_list_entry *widjlist_ptr,
                               t_word             num_entries)
{
    qsort(widjlist_ptr, (size_t) num_entries,
          sizeof(t_widj_list_entry), widgAlphaCompare);
}

/* ***************************************************************************
 *  widgGetListIndex ()
 *****************************************************************************
 * Description:
 *      This function gets the media_id of a media object
 *      and finds its index in the list.
 * Parameters:
 *      t_widj_media_id
 * Globals:
 *
 * Return Values:
 *      t_word
 * Exceptions:
 *
 * Warnings:
 *
 */

LOCAL_FUNC t_word widgGetListIndex (t_widj_media_id media_id,
                         t_widj_list_entry *widjlist_ptr,
                         t_word widjlist_length)
{
    t_word index;
    t_word selected_index;

    for(index=0; index<= widjlist_length;index++)
    {
        if(widjlist_ptr[index].media_id == media_id)
            {
                selected_index=index;
            }
    }
    return selected_index;
}

/* ***************************************************************************
 *  widgDesktopRestart ()
 *****************************************************************************
 * Description:
 *      This function sends the APL_DESKTOP_RESTART_EVENT
 * Parameters:
 *      t_mmi_module_id.
 * Globals:
 *      None.
 * Return Values:
 *      None.
 * Exceptions:
 *
 * Warnings:
 *
 */

GLOBAL_FUNC t_void widgDesktopRestart (t_mmi_module_id mod_id)

{
    AevntSendEvent( APL_DESKTOP_RESTART_EVENT,
                    mod_id,
                    ADSK_MODULE_ID,
                    0,
                    NULL_PTR);
}

/* ***************************************************************************
 *  widgGetwidjListEntries ()
 * ***************************************************************************
 * Description:
 *      This function calls the widjList function to obtain the list entries.
 * Parameters:
 *      t_widg_list_info*
 * Globals:
 *      list_info
 * Return Values:
 *      None.
 * Exceptions:
 *
 * Warnings:
 *
 */
LOCAL_FUNC e_widj_status widgGetwidjListEntries (t_widg_list_info* widglist_ptr)
{
    e_widj_status         widj_result;
    t_widj_list_entry   * widjlist_ptr =NULL_PTR;
    t_word                widjlist_length;
    t_mmi_media_class   * filters_ptr=NULL_PTR;
    t_mmi_media_class     filters[]={
        {MMI_AUDIO,MMI_AUDIO_SUBTYPE_INVALID}
    };

    t_word                filters_length;
    t_mmi_media_class   * handled_ptr=NULL_PTR;
    t_word                handled_length;
    e_widj_device         device_id;
    e_widg_list_type      list_type;

    list_type=widglist_ptr->list_type;

    switch(list_type)
    {
    case SELECT_LIST:
    case SELECT_RADIO_BTN_LIST:
/*check if the media_filters_ptr may become unusable at that stage think of
  allocating a memory for those in select_data_ptr by the widg and then free up those area */
        filters_ptr = select_data_ptr->media_filters_ptr;
        filters_length =  select_data_ptr->media_filters_length;
        handled_ptr = select_data_ptr->media_handled_ptr;
        handled_length =  select_data_ptr->media_handled_length;
        device_id = widj_ALL_DEVICES;

        break;
    case STORE_LIST:
        filters_ptr = NULL_PTR;
        handled_ptr = NULL_PTR;
        filters_length =  0;
        handled_length =  0;
        device_id = widglist_ptr->device_id;
        break;
    case MANAGE_LIST:
        /*         filters_ptr = manage_data_ptr->media_filters_ptr;
                   filters_length =  manage_data_ptr->media_filters_length;*/

        switch(widglist_ptr->title_id)
        {
        case SLPK_PICTURES_TEXT:
            filters[0].media_type=MMI_IMAGE ;
            filters[0].media_subtype=MMI_IMAGE_SUBTYPE_INVALID ;
            break;
        case SLPK_ANIMATIONS_TEXT:
            filters[0].media_type=MMI_VIDEO;
            filters[0].media_subtype=MMI_VIDEO_SUBTYPE_INVALID ;;
            break;
        case SLPK_MELODIES_TEXT:
            filters[0].media_type=MMI_AUDIO;
            filters[0].media_subtype=MMI_AUDIO_SUBTYPE_INVALID ;
            break;
        default:
            break;
        }

        filters_ptr=filters;
        filters_length=sizeof(filters);
        handled_ptr = NULL_PTR;
        handled_length =  0;
        device_id = widj_ALL_DEVICES;

        break;

    default:
        break;

    }

    widj_result = widjList(filters_ptr,
                           filters_length,
                           handled_ptr,
                           handled_length,
                           TRUE,
                           device_id,
                           &widjlist_ptr,
                           &widjlist_length);

    if(widj_OK==widj_result)
    {
        widglist_ptr->widjlist_ptr=widjlist_ptr;
        widglist_ptr->widjlist_length=widjlist_length;
    }

/* widjlist_ptr=NULL_PTR;*/

    return widj_result;
}

/* ***************************************************************************
 *  widgSetPickListInfo ()
 * ***************************************************************************
 * Description:
 *     This fnc sets the picklist info structure required for creating picklist.
 * Parameters:
 *      t_widg_list_info *
 * Globals:
 *      None.
 * Return Values:
 *      None.
 * Exceptions:
 *
 * Warnings:
 *
 */
LOCAL_FUNC t_void widgSetPickListInfo (t_widg_list_info  * widglist_ptr)
{
    t_aglst_info     * picklist_ptr=NULL_PTR;
    t_word             picklist_length;
    t_word             new_offset;
    t_ucs2_string    * new_entry_ptr=NULL_PTR;
    t_word             newentry_ucs2_len;
    t_aglst_info       new_entry_info;
    t_word             new_index=0;
    t_word             index=0;

    new_offset = widglist_ptr->new_offset;

    widglist_ptr->picklist_length = widglist_ptr-> widjlist_length;

    picklist_length = widglist_ptr-> picklist_length;

    picklist_ptr = (t_aglst_info *) our_malloc (picklist_length *
                                                sizeof(t_aglst_info));
    if (picklist_ptr!=NULL_PTR)
    {
        if (new_offset>0)
        {
            newentry_ucs2_len = SlpkMeasureTextIdStrings(1,
                                                         SLPK_NEW_ENTRY_TEXT);

            new_entry_ptr = SlibStringCreate(newentry_ucs2_len);

            if (new_entry_ptr!=NULL_PTR)
            {
                newentry_ucs2_len = SlpkTextIdToUcs2(SLPK_NEW_ENTRY_TEXT,
                                                     new_entry_ptr->start);

                picklist_ptr[new_index].string = * new_entry_ptr;

                new_index++;

                widglist_ptr->new_entry_ptr=new_entry_ptr;
            }
        }

        for (index=new_index; index<picklist_length; index++)
        {
            picklist_ptr[index].string.start =  widglist_ptr->widjlist_ptr
                [index-new_index].media_name;

            picklist_ptr[index].string.length = widglist_ptr->widjlist_ptr
                [index-new_index].media_name_length;
        }

        for (index=0; index<picklist_length; index++)
        {
            picklist_ptr[index].check_state = AGLST_ITEM_STATE_CHECK_OFF;
        }

        for (index=new_index; index<picklist_length; index++)
        {
            e_widj_status     widjinfo_result;
            t_bool            copyrighted;
            t_widj_media_id   media_id;

            media_id=widglist_ptr->widjlist_ptr[index-new_index].media_id;

            widjinfo_result = widjGetMediaObjectInfo(media_id,
                                                     NULL_PTR,
                                                     NULL_PTR,
                                                     NULL_PTR,
                                                     NULL_PTR,
                                                     &copyrighted,
                                                     NULL_PTR,
                                                     NULL_PTR);

            if(widjinfo_result==widj_OK && copyrighted==TRUE)
            {
                picklist_ptr[index].check_state = AGLST_ITEM_STATE_COPYRIGHT;
            }
        }
        widglist_ptr->picklist_ptr = picklist_ptr;
    }
/* picklist_ptr=NULL_PTR;*/
}
/* ***************************************************************************
 *  widgSetSecSoftkey ()
 * ***************************************************************************
 * Description:
 *      This fnc sets the label for the secondary softkey according to the
 *      media type of the current highlighted item.
 * Parameters:
 *      t_widg_list_info  *
 * Globals:
 *      None.
 * Return Values:
 *      (e_slpk_text_id) now: t_bool
 * Exceptions:
 *
 * Warnings:
 *
 */

GLOBAL_FUNC t_bool widgSetSecSoftkey (t_widg_list_info  * widglist_ptr)
{
    t_widj_list_entry   * widjlist_ptr=NULL_PTR;
    t_word                widj_index;
    e_slpk_text_id        sec_softkey;
    t_mmi_media_class     media_class;
    t_bool                update_secsky;

    widjlist_ptr=widglist_ptr->widjlist_ptr;
    widj_index=widglist_ptr->widj_index;

    update_secsky=FALSE;

    if (widglist_ptr->new_offset!=0 && widglist_ptr->selected_index==0)
    {
        sec_softkey=SLPK_NULL_TEXT_ID;
        widglist_ptr->sec_sky_disabled=TRUE;
    }
    else
    {
        if (widjlist_ptr!=NULL_PTR && widjlist_ptr[widj_index].playable!=FALSE)
        {
            media_class = widjlist_ptr[widj_index].media_class;
            switch(media_class.media_type)
            {
            case MMI_MEDIA_TYPE_INVALID:
                sec_softkey=SLPK_NULL_TEXT_ID;
                widglist_ptr->sec_sky_disabled=TRUE;
                break;
            case MMI_AUDIO:
                sec_softkey=SLPK_PLAY_TEXT;
                widglist_ptr->sec_sky_disabled=FALSE;
                break;
            default:
                sec_softkey=SLPK_VIEW_TEXT; /*needs to be checked*/
                widglist_ptr->sec_sky_disabled=FALSE;
                break;
            }
        }

        else
        {
            sec_softkey=SLPK_NULL_TEXT_ID;
            widglist_ptr->sec_sky_disabled=TRUE;
        }
    }

    if (widglist_ptr->sec_softkey != sec_softkey)
    {
        widglist_ptr->sec_softkey = sec_softkey;
        update_secsky=TRUE;
    }

/*    widjlist_ptr=NULL_PTR;*/
    return update_secsky;
}

/* ***************************************************************************
 *  widgSetPrimSoftkey ()
 * ***************************************************************************
 * Description:
 *      This fnc sets the label for the primary softkey according to the
 *      copyright status of the current highlighted item.
 * Parameters:
 *      t_widg_list_info  *
 * Globals:
 *      None.
 * Return Values:
 *      e_slpk_text_id
 * Exceptions:
 *
 * Warnings:
 *
 */
GLOBAL_FUNC t_bool  widgSetPrimSoftkey (t_widg_list_info  * widglist_ptr)
{
    t_aglst_info        * picklist_ptr=NULL_PTR;
    t_word                index;
    e_slpk_text_id        prim_softkey;
    t_bool                update_primsoftkey;

    picklist_ptr=widglist_ptr->picklist_ptr;
    index=widglist_ptr->selected_index;
    update_primsoftkey=FALSE;

    if (picklist_ptr[index].check_state==AGLST_ITEM_STATE_COPYRIGHT &&
        widglist_ptr->show_copyrighted_media==FALSE)
    {
        prim_softkey=SLPK_NULL_TEXT_ID;

        if(widglist_ptr->prim_softkey==prim_softkey)
        {
            update_primsoftkey =FALSE;
        }

        else
        {
            update_primsoftkey =TRUE;
            widglist_ptr->prim_softkey=prim_softkey;
            widglist_ptr->prim_sky_disabled=TRUE;
        }
    }
/*    picklist_ptr=NULL_PTR;*/
    return update_primsoftkey;
}

/* ***************************************************************************
 *  widgCreatePickList ()
 * ***************************************************************************
 * Description:
 *      This fnc. creates the picklist for select, store or manage purposes.
 * Parameters:
 *      t_widg_list_info *
 * Globals:
 *      e_aglst_align list_alignment.
 * Return Values:
 *      t_aglst_handle
 * Exceptions:
 *
 * Warnings:
 *
 */

LOCAL_FUNC t_void widgCreatePickList(t_widg_list_info  * widglist_ptr)
{
    t_aglst_handle    picklist_handle;

    switch(widglist_ptr->list_type)
    {
    case SELECT_LIST:
    case STORE_LIST:
    case MANAGE_LIST:
        picklist_handle = AglstCreatePickList (widglist_ptr->picklist_length,
                                               widglist_ptr->selected_index,
                                               widglist_ptr->picklist_ptr,
                                               TRUE,
                                               list_alignment);
        break;

    case SELECT_RADIO_BTN_LIST:
        picklist_handle = AglstCreateRadioList(widglist_ptr->picklist_length,
                                               widglist_ptr->selected_index,
                                               widglist_ptr->picklist_ptr,
                                               TRUE,
                                               list_alignment,
                                               widglist_ptr->sec_softkey);
        break;

    default:
        break;
    }

  widglist_ptr->picklist_handle=picklist_handle;

  if (NULL_PTR!=picklist_handle && widglist_ptr->title_ptr!=NULL_PTR)
  {
      AglstSetHeader(picklist_handle,
                     widglist_ptr->title_ptr);
  }
}

/* ***************************************************************************
 *  widgSetwidgResult ()
 * ***************************************************************************
 * Description:
 *    This fnc. assigns the widg result value according to widj result.
 * Parameters:
 *    e_widj_status
 * Globals:
 *
 * Return Values:
 *    e_widg_status
 * Exceptions:
 *
 * Warnings:
 *
 */

LOCAL_FUNC e_widg_status widgSetwidgResult(e_widj_status  widj_result)
{
  e_widg_status widg_result;

  switch (widj_result)
    {
      case widj_OK:
          widg_result=widg_OK;
          break;
      case widj_NO_MEDIA_OBJECTS:
          widg_result=widg_LIST_EMPTY;
          break;
      case widj_IO_FAILURE:
      case widj_UNKNOWN_FAILURE:
          widg_result = widg_FAILURE;
          break;
      case widj_UNHANDLED_CONVERSION:
          widg_result = widg_PREVIEW_UNAVAILABLE;
          break;
      case widj_READONLY_MEDIA:
          widg_result = widg_READONLY;
          break;
      default:
      /* widj_NO_REQUESTED_MEDIA,widj_INCORRECT_MEDIA_TYPE,*/
      /* widj_NO_DATA,widj_NO_ENTRIES,widj_INVALID_MEDIA_ID,*/
      /* widj_LOST_MEDIA_OBJECT, widj_MEDIA_OBJECT_EXISTS,*/
      /* widj_NO_FREE_SPACE,widj_INVALID_DEVICE,widj_INVALID_MODULE_ID*/
         widg_result = widg_FAILURE;
         break;
    }
  return widg_result;
}

/* ***************************************************************************
 *  widgCreateSelectList ()
 *****************************************************************************
 * Description:
 *      This function creates a list for selection purposes. Sets the state of
 *      widg_status.list_substate to SELECT_LIST in case of success.
 * Parameters:
 *      None.
 * Globals:
 *      t_widg_select *
 * Return Values:
 *      None.
 * Exceptions:
 *
 * Warnings:
 *
 */

GLOBAL_FUNC t_void widgCreateSelectList (t_widg_select *select_data_ptr)
{
    t_widj_media_id          cur_media_id;
    e_widj_status            widjconv_result;
    e_widj_status            widjlist_result;
    e_widg_status            widglist_result=widg_FAILURE;
    t_word                   widj_list_index;
    t_widg_infotext_info     info_config;
    e_mmi_media_type         media_type;

    if(select_data_ptr!=NULL_PTR)
    {
        list_info.list_type=SELECT_LIST;
        widg_status.list_substate=widg_SELECT_LIST;

        if (select_data_ptr->selected_media_id!=widj_NULL_MEDIA_ID)
        {
            list_info.list_type=SELECT_RADIO_BTN_LIST;
            widg_status.list_substate=widg_SELECT_RADIO_LIST;
        }

        list_info.new_offset=0;
        if (select_data_ptr->show_new_entry)
        {
            list_info.new_offset= widg_NEW_OFFSET;
        }

        list_info.device_id=widj_ALL_DEVICES;

        if (select_data_ptr->title_ptr!=NULL_PTR)
        {
            list_info.title_ptr = (t_ucs2_string*) our_malloc (sizeof(t_ucs2_string));
            list_info.title_ptr->start=select_data_ptr->title_ptr->start;
            list_info.title_ptr->length=select_data_ptr->title_ptr->length;
        }

        widjlist_result= widgGetwidjListEntries (&list_info);

        if (widjlist_result == widj_OK)
        {
            e_widj_status     widjinfo_result;
            t_bool            copyrighted;

            widglist_result=widgSetwidgResult(widjlist_result);

            widgSortList(list_info.widjlist_ptr,list_info.widjlist_length);

            list_info.picklist_length = list_info.widjlist_length+list_info.new_offset;

            widgSetPickListInfo(&list_info);

            list_info.soft_handle=select_soft_handle;

            if (select_data_ptr->selected_media_id != widj_NULL_MEDIA_ID)
            {
                list_info.list_type=SELECT_RADIO_BTN_LIST;

                widg_status.list_substate=widg_SELECT_RADIO_LIST;

                cur_media_id = select_data_ptr->selected_media_id;

                widj_list_index = widgGetListIndex(cur_media_id,list_info.widjlist_ptr,
                                                   list_info.widjlist_length);

                list_info.selected_index = widj_list_index+list_info.new_offset;

                list_info.widj_index = widj_list_index;

                if (list_info.selected_index > list_info.picklist_length)
                {
                    list_info.selected_index = list_info.picklist_length;
                }

                list_info.picklist_ptr[list_info.selected_index].check_state =
                    AGLST_ITEM_STATE_CHECK_ON;

                list_info.soft_handle=NULL_PTR;

               /*should the copyright be checked here ?*/

                widjinfo_result = widjGetMediaObjectInfo(cur_media_id,
                                                         NULL_PTR,
                                                         NULL_PTR,
                                                         NULL_PTR,
                                                         NULL_PTR,
                                                         &copyrighted,
                                                         NULL_PTR,
                                                         NULL_PTR);

                if(widjinfo_result==widj_OK && copyrighted==TRUE)
                {
                    list_info.picklist_ptr[list_info.selected_index].check_state
                                                    = AGLST_ITEM_STATE_COPYRIGHT;
                }
            }

            widgSetSecSoftkey (&list_info); /* disabled/enabled and label info is set into list_info */

            widgSetPrimSoftkey (&list_info); /* disabled/enabled and label info is set into list_info */

            widgCreatePickList(&list_info);
        }

        else
        {
            widglist_result=widgSetwidgResult(widjlist_result);

            if (widjlist_result == widj_NO_MEDIA_OBJECTS)
            {
             /*   info_config.line1=SLPK_EMPTY_LIST_TEXT;*/

                info_config.line1=SLPK_NO_FOUND_1_TEXT;
                info_config.line3=SLPK_NO_FOUND_2_TEXT;

                if(list_info.title_ptr!=NULL_PTR)
                {
                    if(info_config.new_name_ptr==NULL_PTR)
                    {
                        info_config.new_name_ptr=(t_ucs2_string*) our_malloc(sizeof(t_ucs2_string));
                    }

                    info_config.new_name_ptr->start=list_info.title_ptr->start;
                    info_config.new_name_ptr->length=list_info.title_ptr->length;
                }

                if(list_info.title_ptr==NULL_PTR)
                {
                    info_config.line2=SLPK_PICTURES_TEXT;
                   /*info_config.line2=SLPK_OBJECTS_TEXT; */

                    if(info_config.new_name_ptr!=NULL_PTR)
                    {
                        our_free(info_config.new_name_ptr);
                        info_config.new_name_ptr=NULL_PTR;
                    }
                }

                widg_status.list_substate=widg_SELECT_NO_OBJECTS_FOUND;

                widgCreateInfoHandle(&info_config,
                                     widg_status.list_substate,
                                     widg_LIST_MODID);

                widg_status.list_state=WAIT_FOR_TIMER;
            }

            else
            {
                AmanExecRequestStop(widg_LIST_MODID);
            }

            widjComplete(widj_NULL_MEDIA_ID);
/*SelectReturn Event is being sent in StopListHandler*/
        }

        if(select_data_ptr!=NULL_PTR)
        {
            our_free(select_data_ptr);
            select_data_ptr=NULL_PTR;
        }
    }
}

/* ***************************************************************************
 *  widgCreateStoreList ()
 *****************************************************************************
 * Description:
 *      This function creates a list of objects to be stored.
 *      Sets the state of widg_status.list_substate to STORE_LIST on success.
 * Parameters:
 *      None.
 * Globals:
 *      widg_status, store_data_ptr
 * Return Values:
 *      None.
 * Exceptions:
 *
 * Warnings:
 *
 */

GLOBAL_FUNC t_void widgCreateStoreList (t_widg_store *store_data_ptr)
{
    e_widj_status            widjlist_result;
    e_widg_status            widglist_result;

    if(store_data_ptr!=NULL_PTR)
    {
        list_info.device_id = widjMountMediaDevice(store_data_ptr->media_entries_ptr,
                                                   store_data_ptr->media_entries_length);

        if (list_info.device_id!=widj_NO_DEVICES && store_data_ptr->media_entries_length==1)
        {
            list_info.list_type=STORE_LIST;
     /*       widg_status.list_substate=widg_SAVE_MEDIA;  be very careful while sending return events */

/* choose that media id to save without displaying the list */
/* Sema : do not forget it...*/

        }

        if (list_info.device_id!=widj_NO_DEVICES && store_data_ptr->media_entries_length>1)
        {
            widg_status.list_substate=widg_STORE_LIST;
            list_info.list_type=STORE_LIST;
            list_info.new_offset=0;
            list_info.selected_index = 0;
            list_info.widj_index = 0;

            if (store_data_ptr->title_ptr!=NULL_PTR)
            {
                list_info.title_ptr = (t_ucs2_string*) our_malloc (sizeof(t_ucs2_string));
                list_info.title_ptr->start=store_data_ptr->title_ptr->start;
                list_info.title_ptr->length=store_data_ptr->title_ptr->length;
            }

            widjlist_result= widgGetwidjListEntries (&list_info);

            if (widjlist_result == widj_OK)
            {
                widgSortList(list_info.widjlist_ptr,list_info.widjlist_length);

                list_info.picklist_length = list_info.widjlist_length;

                widgSetPickListInfo(&list_info);

                widgCreatePickList(&list_info);

                list_info.soft_handle=store_soft_handle;

            } /*end of widj_LIST==OK*/
        }
        else
        {
            widglist_result=widgSetwidgResult(widjlist_result);

            widjUnmountMediaDevice(list_info.device_id);

            widjComplete(widj_NULL_MEDIA_ID);

            widgStoreReturn(widglist_result);

            AmanExecRequestStop(widg_LIST_MODID);
        }

        if(store_data_ptr!=NULL_PTR)
        {
            our_free(store_data_ptr);
            store_data_ptr=NULL_PTR;
        }
    }
}
/* ***************************************************************************
 *  widgCreateManageList ()
 *****************************************************************************
 * Description:
 *      This function creates a list of objects to be managed.
 *      Sets the state of widg_status.list_substate to MANAGE_LIST on success.
 * Parameters:
 *      None.
 * Globals:
 *      widg_status
 * Return Values:
 *      None.
 * Exceptions:
 *
 * Warnings:
 *
 */

LOCAL_FUNC t_void widgCreateManageList (e_slpk_text_id title_id)
{
/*  t_widg_manage          manage_data; */

    e_widj_status            widjmanage_result;
    e_widg_status            widgmanage_result;
    t_word                   title_ucs2_len;
    t_ucs2_string           *list_title_ptr = NULL_PTR;

    widg_status.manager_substate=widg_MANAGER_LIST;

    mnglist_info.list_type=MANAGE_LIST;

    mnglist_info.device_id=widj_ALL_DEVICES;

    mnglist_info.title_id = title_id;

    title_ucs2_len = SlpkMeasureTextIdStrings(1,title_id);

    list_title_ptr = SlibStringCreate(title_ucs2_len);

    if (list_title_ptr!=NULL_PTR)
    {
        title_ucs2_len = SlpkTextIdToUcs2(title_id,
                                          list_title_ptr->start);
    }

    mnglist_info.title_ptr = (t_ucs2_string*) our_malloc (sizeof(t_ucs2_string));

    mnglist_info.title_ptr->start=list_title_ptr->start;

    mnglist_info.title_ptr->length=list_title_ptr->length;

    mnglist_info.new_offset=0;

    mnglist_info.show_new_entry=FALSE;

    mnglist_info.selected_index = 0;

    mnglist_info.widj_index = 0;

    widjmanage_result= widgGetwidjListEntries (&mnglist_info);

    if (widjmanage_result == widj_OK)
    {
        widgSortList(mnglist_info.widjlist_ptr,
                     mnglist_info.widjlist_length);

        mnglist_info.picklist_length = mnglist_info.widjlist_length;

        widgSetPickListInfo(&mnglist_info);

        widgCreatePickList(&mnglist_info);

        mnglist_info.soft_handle=manage_soft_handle;
    }

    else
    {
        widgmanage_result=widgSetwidgResult(widjmanage_result);

        widjComplete(widj_NULL_MEDIA_ID);

        /*     widgManageReturn(widgmanage_result);*/

        /*return event is being sent in stoplist handler*/

        AmanExecRequestStop(widg_MANAGER_MODID);
    }
    /*     our_free(manage_data_ptr);
        manage_data_ptr=NULL_PTR; */

    /*    SlibStringDestroy(&list_title_ptr); */ /* what does this fnc do?? */
}

/* ***************************************************************************
 *  widgSaveMedia ()
 *****************************************************************************
 * Description:
 *      This function checks the file store and saves the media object passed
 *      by the calling application.
 * Parameters:
 *      None.
 * Globals:
 *      widg_status, save_data_ptr
 * Return Values:
 *      None.
 * Exceptions:
 *
 * Warnings:
 *
 */

LOCAL_FUNC t_void widgSaveMedia (t_widg_save *save_data_ptr)
{
    e_widj_status           widjcheck_result;
    e_widj_status           widjsave_result;
    e_widj_status           widjinfo_result;
    e_widg_status           widglist_result;
    t_widg_infotext_info    info_config;
    t_ucs2                  media_name_ptr[SFILE_MAX_FILENAME_LENGTH];
    t_word                  media_name_length;
    t_bool                  read_only;

    widjinfo_result = widjGetMediaObjectInfo(save_data_ptr->media_id,
                                             media_name_ptr,
                                             &media_name_length,
                                             NULL_PTR,
                                             NULL_PTR,
                                             NULL_PTR,
                                             &read_only,
                                             NULL_PTR);

    if (widjinfo_result==widj_OK && media_name_ptr!=NULL_PTR)
    {
        if(info_config.new_name_ptr==NULL_PTR)
        {
            info_config.new_name_ptr=(t_ucs2_string*) our_malloc(sizeof(t_ucs2_string));
        }

        info_config.new_name_ptr->start=media_name_ptr;
        info_config.new_name_ptr->length=media_name_length;

        if(list_info.new_name_ptr==NULL_PTR)
        {
            list_info.new_name_ptr= (t_ucs2_string*) our_malloc (sizeof(t_ucs2_string));
        }

        list_info.new_name_ptr->start=media_name_ptr;
        list_info.new_name_ptr->length=media_name_length;
    }

    if(widjinfo_result==widj_OK && read_only ==TRUE)
    {
        widg_status.list_substate= widg_SAVE_READONLY;

        info_config.line2=SLPK_IS_READ_ONLY_TEXT;
    }

    if(widjinfo_result==widj_OK && read_only !=TRUE)
    {
        widjcheck_result = widjCheckMediaObjectStore(save_data_ptr->media_id,
                                                     save_data_ptr->media_data_length);

        switch (widjcheck_result)
        {
        case widj_OK:
            widjsave_result = widjSaveMediaObject(save_data_ptr->media_id,
                                                  save_data_ptr->media_class_ptr,
                                                  save_data_ptr->copyrighted,
                                                  save_data_ptr->media_data_ptr,
                                                  save_data_ptr->media_data_length);
            if(widjsave_result==widj_OK)
            {
                widg_status.list_substate= widg_SAVE_SAVED_CNF;

                info_config.line2=SLPK_SAVED_TEXT;
            }

            else
            {
                widg_status.list_substate= widg_SAVE_SAVE_FAILED;

                info_config.line2=SLPK_SAVE_FAILED_TEXT;
            }

            widglist_result = widgSetwidgResult(widjsave_result);

/*get name from widj and form name saved screen*/

            widgCreateInfoHandle(&info_config,
                                 widg_status.list_substate,
                                 widg_LIST_MODID);

             widg_status.list_state=WAIT_FOR_TIMER;

             if (save_data_ptr!=NULL_PTR)
             {
               our_free(save_data_ptr);

               save_data_ptr=NULL_PTR;
             }

            break;

        case widj_NO_FREE_SPACE:

            widg_status.list_substate = widg_SAVE_FREE_STORE_QUERY;

            info_config.line1=SLPK_NO_FREE_SPACE_TEXT;
            info_config.line2=SLPK_DELETE_FILES_QUERY_TEXT;

            widgDisplayQueryScreen(&info_config,widg_SAVE_FREE_STORE_QUERY,widg_LIST_MODID);

            break;

        default:

            widg_status.list_substate= widg_SAVE_SAVE_FAILED;

            info_config.line2=SLPK_SAVE_FAILED_TEXT;

            widglist_result = widgSetwidgResult(widjcheck_result);

            widgCreateInfoHandle(&info_config,
                                 widg_status.list_substate,
                                 widg_LIST_MODID);
            widg_status.list_state=WAIT_FOR_TIMER;

            if (save_data_ptr!=NULL_PTR)
             {
               our_free(save_data_ptr);

               save_data_ptr=NULL_PTR;
             }
            break;
        }
    }
}

/* ***************************************************************************
 *  widgStartConversion ()
 * ***************************************************************************
 * Description:
 *      This function is used for starting conversion of the highlighted
 *      media object in the list.
 * Parameters:
 *      widjlist_ptr and
 *      widj_index .
 * Globals:
 *      t_widg_list_info
 * Return Values:
 *      None.
 * Exceptions:
 *
 * Warnings:
 *
 */

GLOBAL_FUNC t_void widgStartConversion (t_widg_list_info *widglist_ptr,
                                       t_mmi_module_id mod_id)

{
  t_mmi_media_class   media_class;
  t_widj_media_id     media_id;
  e_widj_status       widjconv_result=widj_UNHANDLED_CONVERSION;
  t_word              widj_index;
  t_widj_list_entry  *widjlist_ptr;
  t_word              old_conv_index;
  t_word              widjlist_length;

  widj_index=widglist_ptr->widj_index;
  widjlist_ptr=widglist_ptr->widjlist_ptr;
  widjlist_length=widglist_ptr->widjlist_length;

  if (widglist_ptr->conv_status ==CONVERTING)
  {
      old_conv_index = widgGetListIndex(widglist_ptr->conv_media_id,
                                        widjlist_ptr,
                                        widjlist_length);

/*old_conv_index is absolutely different from the current widj_index*/

      media_class =widjlist_ptr[old_conv_index].media_class;

      widjStopMediaObjectConversion(widglist_ptr->conv_media_id,
                                    &media_class,mod_id);
  }

  if(widjlist_ptr!=NULL_PTR)
  {
      if(widjlist_ptr[widj_index].playable!=FALSE)
      {
          media_id = widjlist_ptr[widj_index].media_id;
          media_class =widjlist_ptr[widj_index].media_class;

          widjComplete(widj_NULL_MEDIA_ID);

          widjconv_result = widjStartMediaObjectConversion(media_id,
                                                           &media_class,
                                                           mod_id);
          if(widjconv_result!=widj_OK)
          {
              widglist_ptr->conv_media_id = widj_INVALID_MEDIA_ID;
              widglist_ptr->conv_status = UNHANDLED;
          }

          if(widjconv_result==widj_OK)
          {
              widglist_ptr->conv_status = CONVERTING;
              widglist_ptr->conv_media_id = media_id;
          }
      }

      else
      {
          widglist_ptr->conv_media_id = widj_INVALID_MEDIA_ID;
          widglist_ptr->conv_status = UNHANDLED;
      }
  }
}

/* ***************************************************************************
 *  widgStopConversion ()
 * ***************************************************************************
 * Description:
 *
 * Parameters:
 *     t_widg_list_info *
 *
 * Globals:
 *
 * Return Values:
 *      None.
 * Exceptions:
 *
 * Warnings:
 *
 */

GLOBAL_FUNC t_void widgStopConversion (t_widg_list_info *widglist_ptr,
                                       t_mmi_module_id mod_id)

{
    t_mmi_media_class   media_class;
    t_widj_media_id     media_id;
    t_word              widj_index;
    t_widj_list_entry * widjlist_ptr;

    widj_index=widglist_ptr->widj_index;

    widjlist_ptr=widglist_ptr->widjlist_ptr;

    if (widglist_ptr->conv_status ==CONVERTING)
    {
        if(widjlist_ptr != NULL_PTR)
        {
            media_id = widjlist_ptr[widj_index].media_id;

            media_class =widjlist_ptr[widj_index].media_class;

            widjStopMediaObjectConversion(widglist_ptr->conv_media_id,
                                          &media_class,
                                          mod_id);
        }
    }

    widglist_ptr->conv_media_id = widj_INVALID_MEDIA_ID;
    widglist_ptr->conv_status = UNHANDLED;

}

/* ***************************************************************************
 *  widgPlayAudio ()
 * ***************************************************************************
 * Description:
 *
 * Parameters:
 *      t_widg_list_info *
 * Globals:
 *
 * Return Values:
 *      None.
 * Exceptions:
 *
 * Warnings:
 *
 */

LOCAL_FUNC t_void widgPlayAudio (t_widg_list_info *widglist_ptr,
                             t_mmi_module_id mod_id)

{
    t_widg_infotext_info      info_config;
    e_widj_status             widj_result;

    if(widglist_ptr->conv_status=CONVERTED)
    {
        widj_result=widjPlayMediaObject(widglist_ptr->conv_media_id,
                                        &play_opts,mod_id);
        if(widj_result!=widj_OK)
        {
            switch(widglist_ptr->list_type)
            {
            case STORE_LIST:
                widg_status.list_substate ==widg_STORE_PREV_UNAVAILABLE;
                break;
            case SELECT_LIST:
            case SELECT_RADIO_BTN_LIST:
                widg_status.list_substate ==widg_SELECT_PREV_UNAVAILABLE;
                break;
            case MANAGE_LIST:
                widg_status.manager_substate ==widg_MANAGER_PREV_UNAVAILABLE;
                break;
            default:
                break;
            }
        }
        else
        {
            info_config.line1=SLPK_PREVIEW_UNAVAILABLE_1_TEXT;
            info_config.line2=SLPK_PREVIEW_UNAVAILABLE_2_TEXT;

            widgCreateInfoHandle(&info_config,
                                 widg_status.list_substate,mod_id);

            if(mod_id ==widg_LIST_MODID)
            {
                widg_status.list_state=WAIT_FOR_TIMER;
            }

            if(mod_id ==widg_MANAGER_MODID)
            {
                widg_status.manager_state=WAIT_FOR_TIMER;
            }
/*the above line will be displaying the preview unavaliable screen */
        }
    }

    if(widglist_ptr->conv_status=CONVERTING && widglist_ptr->conv_status!= UNHANDLED)
    {
        widglist_ptr->playaudio=TRUE;

/* it is assumed that please wait converting screen will not be displayed for audio types */

    }
}

/* ***************************************************************************
 *  widgViewMedia ()
 * ***************************************************************************
 * Description:
 *
 * Parameters:
 *      t_widg_list_info *
 * Globals:
 *
 * Return Values:
 *      None.
 * Exceptions:
 *
 * Warnings:
 *
 */

LOCAL_FUNC t_void widgViewMedia (t_widg_list_info *widglist_ptr,
                             t_mmi_module_id mod_id)
{
    t_mmi_media_class      media_class;

    media_class= widglist_ptr->widjlist_ptr[widglist_ptr->widj_index].
        media_class;

    if(media_class.media_type!=MMI_AUDIO &&
       media_class.media_type!=MMI_MEDIA_TYPE_INVALID)
    {
        AmanExecRequestStart(widg_PREVIEW_MODID);
        widg_status.playing_modid = mod_id;
    }
}

/* ***************************************************************************
 *  widgResetListParams ()
 * ***************************************************************************
 * Description:
 *      This function is used before exiting from the widg_list subapplication.
 *      Makes the necessary init. assignments and frees the memories allocated.
 * Parameters:
 *      t_widg_list_info *, t_mmi_module_id
 * Globals:
 *      widg_status
 * Return Values:
 *      None.
 * Exceptions:
 *
 * Warnings:
 *
 */

LOCAL_FUNC t_void widgResetListParams (t_widg_list_info *widglist_ptr,
                                       t_mmi_module_id mod_id)
{
    widglist_ptr->list_type=NULL_LIST;

    if (widglist_ptr->device_id!=widj_NO_DEVICES)
    {
        widjUnmountMediaDevice(widglist_ptr->device_id);
        widglist_ptr->device_id=widj_NO_DEVICES;
    }

    if(widglist_ptr->title_ptr!=NULL_PTR)
    {
        if(widglist_ptr->title_ptr->start!=NULL_PTR)
        {
            our_free(widglist_ptr->title_ptr->start);
            widglist_ptr->title_ptr->start=NULL_PTR;

            our_free(widglist_ptr->title_ptr);
            widglist_ptr->title_ptr=NULL_PTR;
        }
    }

    if(widglist_ptr->widjlist_ptr!=NULL_PTR)
    {
        our_free(widglist_ptr->widjlist_ptr);
        widglist_ptr->widjlist_ptr=NULL_PTR;
    }

    widglist_ptr->widjlist_length=0;

    if(widglist_ptr->picklist_ptr!=NULL_PTR)
    {
        our_free(widglist_ptr->picklist_ptr);
        widglist_ptr->picklist_ptr=NULL_PTR;
    }

    if(widglist_ptr->new_entry_ptr!=NULL_PTR)
    {
        SlibStringDestroy(&widglist_ptr->new_entry_ptr);
        widglist_ptr->new_entry_ptr=NULL_PTR;
    }

    widglist_ptr->picklist_length=0;
    widglist_ptr->picklist_handle=NULL_PTR;
    widglist_ptr->new_offset=0;
    widglist_ptr->selected_index=0;
    widglist_ptr->widj_index=0;
    widglist_ptr->sec_softkey=SLPK_NULL_TEXT_ID;
    widglist_ptr->conv_status=UNHANDLED;
    widglist_ptr->conv_media_id=widj_INVALID_MEDIA_ID;
    widglist_ptr->number_of_media=0;
    widglist_ptr->number_of_stored=0;
    widglist_ptr->number_of_deleted=0;

    if(widglist_ptr->old_name_ptr!=NULL_PTR)
    {
        if(widglist_ptr->old_name_ptr->start!=NULL_PTR)
        {
            our_free(widglist_ptr->old_name_ptr->start);

            widglist_ptr->old_name_ptr->start=NULL_PTR;

            our_free(widglist_ptr->old_name_ptr);

            widglist_ptr->old_name_ptr=NULL_PTR;

        }
    }

    if(widglist_ptr->new_name_ptr!=NULL_PTR)
    {
        if(widglist_ptr->new_name_ptr->start!=NULL_PTR)
        {
            our_free(widglist_ptr->new_name_ptr->start);

            widglist_ptr->new_name_ptr->start=NULL_PTR;

            our_free(widglist_ptr->new_name_ptr);

            widglist_ptr->new_name_ptr=NULL_PTR;
        }
    }

    switch(mod_id)
    {
    case widg_LIST_MODID:
        widg_status.list_event = APL_NULL_EVENT;
        widg_status.list_src_modid = MMI_NULL_MODULE_ID;
        widg_status.list_substate = widg_LIST_IDLE;

        if(select_data_ptr!=NULL_PTR)
        {
            our_free(select_data_ptr);
            select_data_ptr=NULL_PTR;
        }

        if(store_data_ptr!=NULL_PTR)
        {
            our_free(store_data_ptr);
            store_data_ptr=NULL_PTR;
        }

        if(save_data_ptr!=NULL_PTR)
        {
            our_free(save_data_ptr);
            save_data_ptr=NULL_PTR;
        }

    case widg_MANAGER_MODID:
        widg_status.manager_event = APL_NULL_EVENT;
        widg_status.mng_src_modid = MMI_NULL_MODULE_ID;
        widg_status.manager_substate = widg_MANAGER_IDLE;
        break;

/*       if(manage_data_ptr!=NULL_PTR)
         {
           our_free(manage_data_ptr);
           manage_data_ptr!=NULL_PTR;
           }*/

    default:
        break;
    }
}

/* ***************************************************************************
 *  widgStopListHandler ()
 * ***************************************************************************
 * Description:
 *
 * Parameters:
 *      None.
 * Globals:
 *
 * Return Values:
 *      None.
 * Exceptions:
 *
 * Warnings:
 *
 */

LOCAL_FUNC t_void widgStopListHandler (t_widg_list_info *widglist_ptr, t_mmi_module_id mod_id)

{
    if (widglist_ptr->list_type==SAVE_MEDIA && mod_id == widg_LIST_MODID)
    {
        if(save_data_ptr!=NULL_PTR)
        {
            our_free(save_data_ptr);
            save_data_ptr=NULL_PTR;
        }

        widgSaveReturn(widglist_result);

        widglist_ptr->list_type==NULL_LIST;
        widg_status.list_event = APL_NULL_EVENT;
        widg_status.list_src_modid = MMI_NULL_MODULE_ID;
        widg_status.list_substate = widg_LIST_IDLE;
    }

    if (widglist_ptr->list_type==MANAGER_MENU_STATE && mod_id == widg_MANAGER_MODID)
    {
        widgManageReturn(widgmanage_result);
        widgDestroyGuiObjects(mod_id);
   /*     widgResetListParams(widglist_ptr,mod_id); */
    }

    else
    {
        switch (widglist_ptr->list_type)
        {
        case SELECT_LIST:
        case SELECT_RADIO_BTN_LIST:
            widgSelectReturn(widglist_result);
            break;
        case STORE_LIST:
            widgStoreReturn(widglist_result);
            break;
        case MANAGE_LIST:
            widgManageReturn(widgmanage_result);
            break;
        default:
            break;
        }

        widgStopConversion(widglist_ptr,mod_id);
        /*       widjComplete(); send widjComplete before you came here */

        widgDestroyGuiObjects(mod_id);

        if(widglist_ptr->picklist_handle!=NULL_PTR)
        {
            AglstDestroy(&widglist_ptr->picklist_handle);
        }
        widgResetListParams(widglist_ptr,mod_id);
    }
}

/* ***************************************************************************
 *  widgSuspendList ()
 * ***************************************************************************
 * Description:
 *      This function is used for suspending the widg_list subapplication.
 *      Makes the necessary assignments and suspends picklist and so...
 * Parameters:
 *      None.
 * Globals:
 *      widg_status
 * Return Values:
 *      None.
 * Exceptions:
 *
 * Warnings:
 *
 */

LOCAL_FUNC t_void widgSuspendList (t_widg_list_info *widglist_ptr,
                                   e_widg_subapp_substates  subapp_substate,
                                   t_mmi_module_id mod_id)

{
    if (mod_id==widg_LIST_MODID)
    {
        widg_status.list_state = SUSPENDED;
    }
    if (mod_id==widg_MANAGER_MODID)
    {
        widg_status.manager_state = SUSPENDED;
    }

/* this fnc. will not be called if the manager subapp is in the menu state and the necessary fncs will be done there. */

    switch (subapp_substate)
    {
    case widg_SELECT_LIST:
    case widg_MANAGER_LIST:
    case widg_STORE_LIST:
        if (widglist_ptr->soft_handle!=NULL_PTR)
        {
            (t_void) AgskyDisableSoftarea(widglist_ptr->soft_handle);
            (t_void) AgskyDisplaySoftarea(widglist_ptr->soft_handle);
        }
    case widg_SELECT_RADIO_LIST:
        widgStopConversion(widglist_ptr,mod_id);
        if (widglist_ptr->picklist_handle!=NULL_PTR)
        {
            AglstSuspend(widglist_ptr->picklist_handle);
        }
        break;

    case widg_SELECT_WAIT_CONV_PREVIEW:
    case widg_STORE_WAIT_CONV_PREVIEW:
    case widg_MANAGER_WAIT_CONV_PREVIEW:
        if (wait_soft_handle!=NULL_PTR)
        {
            (t_void) AgskyDisableSoftarea(wait_soft_handle);
            (t_void) AgskyDisplaySoftarea(wait_soft_handle);
        }
        widgStopConversion(widglist_ptr,mod_id);
        if (widglist_ptr->picklist_handle!=NULL_PTR)
        {
            AglstSuspend(widglist_ptr->picklist_handle);
        }
        widgSetListState(mod_id,FALSE);
        break;

    case widg_SELECT_WAIT_CONV_RETURN: /*decide whether to suspend or stop the application at this pnt */
/*    case widg_MANAGER_WAIT_DELETE_ALL:*/
        if (wait_soft_handle!=NULL_PTR)
        {
            (t_void)  AgskyDisableSoftarea(wait_soft_handle);
            (t_void)  AgskyDisplaySoftarea(wait_soft_handle);
        }
        if (widglist_ptr->picklist_handle!=NULL_PTR)
        {
            AglstSuspend(widglist_ptr->picklist_handle);
        }
        break;

    case widg_SELECT_PREV_UNAVAILABLE:
    case widg_STORE_PREV_UNAVAILABLE:
    case widg_STORE_ALREADY_EXISTS:   /*widg will not return to to re-edit if it is suspended*/
    case widg_STORE_SAVED_CNF:
    case widg_STORE_READONLY:
    case widg_STORE_SAVE_FAILED:
    case widg_MANAGER_PREV_UNAVAILABLE:
    case widg_MANAGER_RENAMED_TO:    /* Renamed To, Rename Failed */
    case widg_MANAGER_RENAME_FAILED: /* widj returned invalid media_id */
    case widg_MANAGER_RENAME_ALREADY_EXISTS: /* widg will not return to to re-edit if it is suspended */
    case widg_MANAGER_DELETED_CNF:
    case widg_MANAGER_DELETE_FAILED:
    case widg_MANAGER_DELETE_READONLY:
    case widg_MANAGER_SOME_DELETED_CNF:
        widgTimerHandler(widg_STOP_INFO_SCREEN_TIMER, mod_id);
        if (widglist_ptr->picklist_handle!=NULL_PTR)
        {
            AglstSuspend(widglist_ptr->picklist_handle);
        }
        widgSetListState(mod_id,FALSE);
        break;

    case widg_SELECT_PREV_UNAV_RETURN:
    case widg_SELECT_NO_OBJECTS_FOUND:
    case widg_SAVE_SAVE_FAILED:
    case widg_SAVE_READONLY:
    case widg_SAVE_SAVED_CNF:
    case widg_MANAGER_ALL_DELETED_CNF:
        widgTimerHandler(widg_STOP_INFO_SCREEN_TIMER, mod_id);
        AmanExecRequestStop(mod_id);
        break;

    case widg_MANAGER_RENAME_EDIT:
        if (widglist_ptr->picklist_handle!=NULL_PTR)
        {
            AglstSuspend(widglist_ptr->picklist_handle);
        }

        if (rename_alpha_handle!=NULL_PTR)
        {
            AgeditAlphaSuspend(rename_alpha_handle);
        }
      break;

    case widg_STORE_SAVE_AS_EDIT:
        if (widglist_ptr->picklist_handle!=NULL_PTR)
        {
            AglstSuspend(widglist_ptr->picklist_handle);
        }
        if (save_alpha_handle!=NULL_PTR)
        {
            AgeditAlphaSuspend(save_alpha_handle);
        }
      break;

    case widg_SAVE_FREE_STORE_QUERY:
        if (query_soft_handle!=NULL_PTR)
        {
            (t_void) AgskyDisableSoftarea(query_soft_handle);
            (t_void) AgskyDisplaySoftarea(query_soft_handle);
        }
        widg_status.list_substate=widg_SAVE_MEDIA;
        break;

    case widg_STORE_FREE_STORE_QUERY:
        if (query_soft_handle!=NULL_PTR)
        {
            (t_void) AgskyDisableSoftarea(query_soft_handle);
            (t_void) AgskyDisplaySoftarea(query_soft_handle);
        }

        if (widglist_ptr->picklist_handle!=NULL_PTR)
        {
            AglstSuspend(widglist_ptr->picklist_handle);
        }

        widg_status.list_substate=widg_STORE_SAVE_AS_EDIT;
        break;

    case widg_MANAGER_DELETE_QUERY:
    case widg_MANAGER_DELETEALL_QUERY:
        if (query_soft_handle!=NULL_PTR)
        {
            (t_void) AgskyDisableSoftarea(query_soft_handle);
            (t_void) AgskyDisplaySoftarea(query_soft_handle);
        }

        if (widglist_ptr->picklist_handle!=NULL_PTR)
        {
            AglstSuspend(widglist_ptr->picklist_handle);
        }
        /*   widgSetListState(mod_id,FALSE); the state will not be changed */
        break;
    default:
        break;
/* no save_media state can exist in above cases, it already means list is suspended */
    }
}

/* ***************************************************************************
 *  widgResumeList ()
 * ***************************************************************************
 * Description:
 *      This function is used for resuming the list subapps
 * Parameters:
 *      None.
 * Globals:
 *
 * Return Values:
 *      None.
 * Exceptions:
 *
 * Warnings:
 *
 */

LOCAL_FUNC t_void widgResumeList (t_widg_list_info *widglist_ptr,
                                  e_widg_subapp_substates  subapp_substate,
                                  t_mmi_module_id mod_id)

{
    if (mod_id==widg_LIST_MODID)
    {
        widg_status.list_state = RUNNING;
    }

    if (mod_id==widg_MANAGER_MODID)
    {
        widg_status.manager_state = RUNNING;
    }

    switch (subapp_substate)
    {
    case widg_SELECT_LIST:
    case widg_STORE_LIST:
    case widg_MANAGER_LIST:
        if (widglist_ptr->soft_handle!=NULL_PTR)
        {
            (t_void) AgskyEnableSoftarea(widglist_ptr->soft_handle);
            (t_void) AgskyDisplaySoftarea(widglist_ptr->soft_handle);
        }
    case widg_SELECT_RADIO_LIST:
        widgStartConversion(widglist_ptr,mod_id);
        if (widglist_ptr->picklist_handle!=NULL_PTR)
        {
            AglstResume(widglist_ptr->picklist_handle);

            AglstDisplay(widglist_ptr->picklist_handle);
        }
        break;

    case widg_SELECT_WAIT_CONV_RETURN:
/*    case widg_MANAGER_WAIT_DELETE_ALL:*/
        if (wait_soft_handle!=NULL_PTR)
        {
            (t_void) AgskyEnableSoftarea(wait_soft_handle);
            (t_void) AgskyDisplaySoftarea(wait_soft_handle);
        }
        widgDisplayWaitScreen(subapp_substate,mod_id);
        if (widglist_ptr->picklist_handle!=NULL_PTR)
        {
            AglstResume(widglist_ptr->picklist_handle);
        }
        break;

    case widg_SAVE_MEDIA:
        widgSaveMedia(save_data_ptr);
        break;

    case widg_MANAGER_RENAME_EDIT:
        if (widglist_ptr->picklist_handle!=NULL_PTR)
        {
            AglstResume(widglist_ptr->picklist_handle);
        }
/*text editor will be displayed with any key press */

      if(rename_alpha_handle!=NULL_PTR)
      {
          AlibClearArea(ALIB_CLEAR_MAINAREA);
          AlibClearArea(ALIB_CLEAR_SOFTAREA);
          AgeditAlphaDisplay(rename_alpha_handle);
      }
      break;

    case widg_STORE_SAVE_AS_EDIT:
        if (widglist_ptr->picklist_handle!=NULL_PTR)
        {
            AglstResume(widglist_ptr->picklist_handle);
        }

        if(save_alpha_handle!=NULL_PTR)
        {
            AlibClearArea(ALIB_CLEAR_MAINAREA);
            AlibClearArea(ALIB_CLEAR_SOFTAREA);
            AgeditAlphaDisplay(save_alpha_handle);
        }
      break;

    case widg_MANAGER_DELETE_QUERY:
    case widg_MANAGER_DELETEALL_QUERY:
        if (query_soft_handle!=NULL_PTR)
        {
            (t_void) AgskyEnableSoftarea(query_soft_handle);
            (t_void) AgskyDisplaySoftarea(query_soft_handle);
        }
        if (widglist_ptr->picklist_handle!=NULL_PTR)
        {
            AglstResume (widglist_ptr->picklist_handle);
        }
        break;

      default:
        break;
    }
}

/* ***************************************************************************
 *  widgSelectReturn ()
 * ***************************************************************************
 * Description:
 *
 *
 * Parameters:
 *
 * Globals:
 *
 * Return Values:
 *
 * Exceptions:
 *
 * Warnings:
 *    Should only be called by widg_LIST_MODID.
 */

LOCAL_FUNC t_void widgSelectReturn (e_widg_status select_status)

{
    t_widg_select_return   select_return;

    select_return.widg_status=select_status;

    if(select_status==widg_OK)
    {
        select_return.media_id = list_info.conv_media_id;
    }
    else
    {
        select_return.media_id = widj_NULL_MEDIA_ID;
    }

    AevntSendEvent( APL_UON_SELECT_RETURN_EVENT,
                    widg_LIST_MODID,
                    widg_status.list_src_modid,
                    sizeof(t_widg_select_return),
                    &select_return);
}

/* ***************************************************************************
 *  widgSaveReturn ()
 * ***************************************************************************
 * Description:
 *
 *
 * Parameters:
 *
 * Globals:
 *
 * Return Values:
 *
 * Exceptions:
 *
 * Warnings:
 *
 */

LOCAL_FUNC t_void widgSaveReturn (e_widg_status save_status)

{
    t_widg_save_return   save_return;
    save_return.widg_status =save_status;

    AevntSendEvent( APL_UON_SAVE_RETURN_EVENT,
                    widg_LIST_MODID,
                    widg_status.list_src_modid,
                    sizeof(t_widg_save_return),
                    &save_return);
}

/* ***************************************************************************
 *  widgStoreReturn ()
 * ***************************************************************************
 * Description:
 *
 *
 * Parameters:
 *
 * Globals:
 *
 * Return Values:
 *
 * Exceptions:
 *
 * Warnings:
 *
 */

LOCAL_FUNC t_void widgStoreReturn (e_widg_status store_status)

{
    t_widg_store_return  store_return;
    t_word               i;

    store_return.widg_status = store_status;

    store_return.media_stored_ptr=list_info.stored_ptr;

    store_return.media_stored_length = list_info.number_of_media;

/*  the store_return structure may be filled in during the event processing */
    AevntSendEvent( APL_UON_STORE_RETURN_EVENT,
                    widg_LIST_MODID,
                    widg_status.list_src_modid,
                    sizeof(t_widg_store_return),
                    &store_return);
}

/* ***************************************************************************
 *  widgProcessGuiListResults ()
 * ***************************************************************************
 * Description:
 *
 *
 * Parameters:
 *
 * Globals:
 *
 * Return Values:
 *
 * Exceptions:
 *
 * Warnings:
 *
 */

LOCAL_FUNC t_void widgProcessGuiListResults (t_mmi_event_id    event_id,
                                             t_widg_list_info* widglist_ptr,
                                             t_mmi_module_id   mod_id,
                                             e_widg_status   * widg_result_ptr)

{
    t_widg_gui_status        * gui_status_ptr;
    t_ucs2                    *media_name_ptr = NULL_PTR;
    t_word                     media_name_length;

    t_widg_infotext_info       info_config;

    gui_status_ptr=widgGuiListEventHandler(event_id,widglist_ptr,mod_id);

    switch(gui_status_ptr->gui_result)
    {
    case widg_GUI_EVENT_PROCESSED: /* up/down events */
        break;

    case widg_GUI_USER_EXITED:
        *widg_result_ptr=widg_USER_EXITED;
        AmanExecRequestStop(mod_id);
        break;

    case widg_GUI_USER_QUIT:
        *widg_result_ptr=widg_USER_QUIT;
        widgDesktopRestart(mod_id);
        break;

    case widg_GUI_ITEM_SELECTED:
        switch(gui_status_ptr->selected)
        {
        case SLPK_VIEW_TEXT: /*it must be assured that sec sky is not assigned as view, when the current item is of audio type */

            if(widglist_ptr->conv_status=CONVERTED)
            {
                AmanExecRequestStart(widg_PREVIEW_MODID);
                widg_status.playing_modid = mod_id;
            }

            else
            {
                widgSetListState(mod_id,TRUE); /* widg_status.list_substate or manager_substate is being set here */

                if(mod_id==widg_LIST_MODID)
                {
                    widgDisplayWaitScreen(widg_status.list_substate,
                                          widg_LIST_MODID);
                }

                if(mod_id==widg_MANAGER_MODID)
                {
                    widgDisplayWaitScreen(widg_status.manager_substate,
                                          widg_MANAGER_MODID);
                }
            }
            break;

        case SLPK_PLAY_TEXT:
            widgPlayAudio(widglist_ptr,mod_id);
            break;

        case SLPK_OK_TEXT: /*TICK */    /*store and manage list should not fall in this case*/

            if (widglist_ptr->list_type==SELECT_LIST || widglist_ptr->list_type==SELECT_RADIO_BTN_LIST)
            {
                widgDestroyGuiObjects(mod_id);

                if (widglist_ptr->picklist_handle!=NULL_PTR)
                {
                    AglstDestroy(&widglist_ptr->picklist_handle);
                }

                switch(widglist_ptr->conv_status)
                {
                case UNHANDLED:

                    if(widglist_ptr->selected_index==0 && widglist_ptr->new_offset !=0)
                    {
                        *widg_result_ptr = widg_NEW_ENTRY;
                    }
                    else
                    {
                        *widg_result_ptr = widg_PREVIEW_UNAVAILABLE;
                    }

           /*  widjComplete(widglist_ptr->conv_media_id); the conversion should not being done for the unhandled media*/

                    AmanExecRequestStop(mod_id);

                    break;

                case CONVERTING:

                    *widg_result_ptr = widg_OK;

                    if(widglist_ptr->list_type==SELECT_LIST || widglist_ptr->list_type==SELECT_RADIO_BTN_LIST)
                    {
                        widg_status.list_substate=widg_SELECT_WAIT_CONV_RETURN;
                    }
                    break;

                case CONVERTED:
                   *widg_result_ptr=widg_OK;
                    widjComplete(widglist_ptr->conv_media_id);
                    AmanExecRequestStop(mod_id);
                    break;

                default:
                    break;
                }
            }

         /* widg does not come here while in Manager_Menu_State*/

            break;

        case SLPK_SAVE_TEXT:

            if(widglist_ptr->list_type==STORE_LIST)
            {
                t_word     widj_index;

                widg_status.list_substate=widg_STORE_SAVE_AS_EDIT; /*widgStateHandler */

                widj_index=widglist_ptr->widj_index;

                media_name_ptr = widglist_ptr->widjlist_ptr[widj_index].media_name;
                media_name_length = widglist_ptr->widjlist_ptr[widj_index].media_name_length;

                if(widglist_ptr->old_name_ptr==NULL_PTR)
                {
                    widglist_ptr->old_name_ptr= (t_ucs2_string*) our_malloc (sizeof(t_ucs2_string));
                }

                widglist_ptr->old_name_ptr->start =  media_name_ptr;

                widglist_ptr->old_name_ptr->length = media_name_length;

                if(widglist_ptr->new_name_ptr==NULL_PTR)
                {
                    widglist_ptr->new_name_ptr= (t_ucs2_string*) our_malloc (sizeof(t_ucs2_string));
                }

                widglist_ptr->new_name_ptr->start = media_name_ptr;

                widglist_ptr->new_name_ptr->length = media_name_length;

                widgDisplayEditScreen(widglist_ptr->new_name_ptr->start,
                                      widglist_ptr->new_name_ptr->length,
                                      widg_STORE_SAVE_AS_EDIT);

                media_name_ptr=NULL_PTR;
            }

            break;

  /*      case SLPK_MENU_TEXT:  This case should not be entered since Softarea should send popup_opened result instead of item selected.
            widglist_ptr->sec_softkey=SLPK_NULL_TEXT_ID;
            widglist_ptr->sec_sky_disabled=TRUE;
            widglist_ptr->prim_softkey=SLPK_SELECT_TEXT;
            widglist_ptr->prim_sky_disabled=FALSE;

            widgUpdateSoftarea(widglist_ptr->soft_handle,
                               TRUE,
                               TRUE,
                               SLPK_SELECT_TEXT,
                               SLPK_NULL_TEXT_ID);

            popup menu will be displayed and the primlabel should be changed to select, secsky will be disabled*/

        case SLPK_RENAME_TEXT:

            if(widglist_ptr->list_type==MANAGE_LIST)
            {
                t_word            widj_index;
                t_widj_media_id   cur_media_id;
                t_bool            read_only;
                e_widj_status     widjinfo_result;

                widj_index = mnglist_info.widj_index;

                cur_media_id=mnglist_info.widjlist_ptr[widj_index].media_id;

                widjinfo_result = widjGetMediaObjectInfo(cur_media_id,
                                                         NULL_PTR,
                                                         NULL_PTR,
                                                         NULL_PTR,
                                                         NULL_PTR,
                                                         NULL_PTR,
                                                         &read_only,
                                                         NULL_PTR);

                if(widjinfo_result==widj_OK && read_only==TRUE)
                {
                   /* can read-only objects be renamed */
                }

                if(widjinfo_result==widj_OK && read_only!=TRUE)
                {
                    widg_status.manager_substate=widg_MANAGER_RENAME_EDIT;
                    widj_index=widglist_ptr->widj_index;

                    media_name_ptr = widglist_ptr->widjlist_ptr[widj_index].media_name;
                    media_name_length = widglist_ptr->widjlist_ptr[widj_index].media_name_length;

                    if(widglist_ptr->old_name_ptr==NULL_PTR)
                    {
                        widglist_ptr->old_name_ptr= (t_ucs2_string*) our_malloc (sizeof(t_ucs2_string));
                    }

                    widglist_ptr->old_name_ptr->start =  media_name_ptr;

                    widglist_ptr->old_name_ptr->length = media_name_length;

                    if(widglist_ptr->new_name_ptr==NULL_PTR)
                    {
                        widglist_ptr->new_name_ptr= (t_ucs2_string*) our_malloc (sizeof(t_ucs2_string));
                    }

                    widglist_ptr->new_name_ptr->start = media_name_ptr;

                    widglist_ptr->new_name_ptr->length = media_name_length;

                    widgDisplayEditScreen(widglist_ptr->new_name_ptr->start,
                                          widglist_ptr->new_name_ptr->length,
                                          widg_MANAGER_RENAME_EDIT);

                    media_name_ptr=NULL_PTR;
                }
            }
            break;

        case SLPK_DELETE_TEXT:

            if(widglist_ptr->list_type==MANAGE_LIST)
            {
                t_word      name_length;
                t_ucs2    * name_ptr;
                t_word      index;

                widg_status.manager_substate=widg_MANAGER_DELETE_QUERY;

                widgStopConversion (widglist_ptr, mod_id);

                info_config.line1=SLPK_DELETE_QUERY_TEXT;

                index = mnglist_info.selected_index;

                name_ptr=mnglist_info.widjlist_ptr[index].media_name;

                name_length=mnglist_info.widjlist_ptr[index].media_name_length;

                if(info_config.old_name_ptr==NULL_PTR)
                {
                    info_config.old_name_ptr= (t_ucs2_string*) our_malloc (sizeof(t_ucs2_string));
                }

                info_config.old_name_ptr->start=name_ptr;

                info_config.old_name_ptr->length=name_length;

                widgDisplayQueryScreen(&info_config,
                                       widg_MANAGER_DELETE_QUERY,
                                       widg_MANAGER_MODID);
            }

            break;

        case SLPK_DELETE_ALL_TEXT:
            if(widglist_ptr->list_type==MANAGE_LIST)
            {
                widg_status.manager_substate=widg_MANAGER_DELETEALL_QUERY;
                widgStopConversion (widglist_ptr, mod_id);

                info_config.line1=SLPK_DELETE_ALL_QUERY_TEXT;

                info_config.line2=mnglist_info.title_id;

                widgDisplayQueryScreen(&info_config,
                                       widg_MANAGER_DELETEALL_QUERY,
                                       widg_MANAGER_MODID);
            }
            break;

    /*    case SLPK_CANCEL_TEXT: may only come for manager list at this state and removed from the popup items.

            break; */

        default:
            break;
        }
        break;
    default:
        break;
    }
}

/* ***************************************************************************
 *  widgListEventHandler ()
 * ***************************************************************************
 * Description:
 *
 *
 * Parameters:
 *
 * Globals:
 *
 * Return Values:
 *
 * Exceptions:
 *
 * Warnings:
 *
 */

LOCAL_FUNC t_void widgListEventHandler (t_mmi_event_id event_id,
                                        t_widg_list_info * widglist_ptr,
                                        e_widg_status * widg_result_ptr,
                                        t_mmi_module_id mod_id)

{
    if (EVENT_CLASS(event_id)==APL_KEY_DEPRESSED_EVENT_CLASS ||
        EVENT_CLASS(event_id)== APL_KEY_CONTINUED_EVENT_CLASS ||
        EVENT_CLASS(event_id)==APL_KEY_RELEASED_EVENT_CLASS ||
        event_id== APL_LIBRARY_SCROLL_TIMER_EVENT)
    {
        widgProcessGuiListResults (event_id,
                                   widglist_ptr,
                                   mod_id,
                                   widg_result_ptr);
    }

    if (widglist_ptr->conv_status==CONVERTING && event_id==SPT_widj_MEDIA_CONVERSION_EVENT)
    {
        t_mmi_media_class           media_class;
        widglist_ptr->conv_status = CONVERTED;

        media_class = widglist_ptr->widjlist_ptr[widglist_ptr->widj_index].media_class;

        if(media_class.media_type==MMI_AUDIO &&
           media_class.media_type!=MMI_MEDIA_TYPE_INVALID &&
           widglist_ptr->playaudio==TRUE)
        {
            widgPlayAudio(widglist_ptr,mod_id);
        }
    }
}

/* ***************************************************************************
 *  widgWaitEventHandler ()
 * ***************************************************************************
 * Description:
 *
 *
 * Parameters:
 *
 * Globals:
 *
 * Return Values:
 *
 * Exceptions:
 *
 * Warnings:
 *
 */

LOCAL_FUNC t_void widgWaitEventHandler (t_mmi_event_id event_id,
                                        t_widg_list_info * widglist_ptr,
                                        e_widg_status * widg_result_ptr,
                                        t_mmi_module_id mod_id)

{
    t_widg_gui_status    * gui_status_ptr;

    if (EVENT_CLASS(event_id)==APL_KEY_DEPRESSED_EVENT_CLASS ||
        EVENT_CLASS(event_id)== APL_KEY_CONTINUED_EVENT_CLASS ||
        EVENT_CLASS(event_id)==APL_KEY_RELEASED_EVENT_CLASS ||
        event_id== APL_LIBRARY_SCROLL_TIMER_EVENT)
    {
        gui_status_ptr=widgGuiWaitEventHandler(event_id,widglist_ptr,mod_id);
    }
    switch(gui_status_ptr->gui_result)
    {
    case widg_GUI_USER_QUIT:
        * widg_result_ptr=widg_USER_QUIT;
    widgDesktopRestart(mod_id);
    break;
    case widg_GUI_USER_EXITED:
        widgSetListState(mod_id,FALSE);
        widgDisplayListScreen(widglist_ptr->list_type,
                              widglist_ptr->sec_softkey);
/* info handles are destroyed at the display handler whenever they are created,
   so there is no need to destroy info_handle */
        break;
    default:
        break;
    }

    if(event_id==SPT_widj_MEDIA_CONVERSION_EVENT)
    {
        widglist_ptr->conv_status=CONVERTED;
        widgViewMedia(widglist_ptr,mod_id);
/*we are not expected to be at that state for audio type media objects */
        widgSetListState(mod_id,FALSE);
    }
}

/* ***************************************************************************
 *  widgInfoEventHandler ()
 * ***************************************************************************
 * Description:
 *
 *
 * Parameters:
 *
 * Globals:
 *
 * Return Values:
 *
 * Exceptions:
 *
 * Warnings:
 *
 */

LOCAL_FUNC t_void widgInfoEventHandler (t_widg_list_info* widglist_ptr,
                                        t_mmi_event_id event_id,
                                        t_mmi_module_id mod_id)
{
   t_bool   chg_primsky;
   t_bool   chg_secsky;

    if ( EVENT_CLASS(event_id)==APL_KEY_DEPRESSED_EVENT_CLASS ||
         EVENT_CLASS(event_id)== APL_KEY_CONTINUED_EVENT_CLASS ||
         EVENT_CLASS(event_id)==APL_KEY_RELEASED_EVENT_CLASS ||
         event_id == APL_UON_INFO_TIMER_EVENT)
    {
        widgTimerHandler(widg_STOP_INFO_SCREEN_TIMER, mod_id);
        AlibAcknowledgeKey(event_id); /*this is unnecessary for timer_event) */
        widgSetListState(mod_id,FALSE);

        chg_secsky = widgSetSecSoftkey(widglist_ptr);

        if (widglist_ptr->list_type==SELECT_LIST)
        {
           chg_primsky = widgSetPrimSoftkey(widglist_ptr);
        }

        if (widglist_ptr->list_type==STORE_LIST)
        {
          t_word    index;

          index=list_info.selected_index;

          if (list_info.picklist_ptr[index].check_state==AGLST_ITEM_STATE_STORED)
          {
             chg_primsky = TRUE;
             widglist_ptr->prim_softkey=SLPK_NULL_TEXT_ID;
             widglist_ptr->prim_sky_disabled=TRUE;
          }
          else
          {
             chg_primsky = TRUE;
             widglist_ptr->prim_softkey=SLPK_SAVE_TEXT;
             widglist_ptr->prim_sky_disabled=FALSE;
          }
        }

        widgUpdateSoftarea(widglist_ptr->soft_handle,
                           chg_primsky,
                           chg_secsky,
                           widglist_ptr->sec_softkey,
                           widglist_ptr->prim_softkey);

        widgDisplayListScreen(widglist_ptr->list_type,widglist_ptr->sec_softkey);
    }
}

/* ***************************************************************************
 *  widgStopSubappEventHandler ()
 * ***************************************************************************
 * Description:
 *
 *
 * Parameters:
 *
 * Globals:
 *
 * Return Values:
 *
 * Exceptions:
 *
 * Warnings:
 *
 */

LOCAL_FUNC t_void widgStopSubappEventHandler(t_mmi_event_id event_id,
                                             t_mmi_module_id mod_id)
{
    if ( EVENT_CLASS(event_id)==APL_KEY_DEPRESSED_EVENT_CLASS ||
         EVENT_CLASS(event_id)== APL_KEY_CONTINUED_EVENT_CLASS ||
         EVENT_CLASS(event_id)==APL_KEY_RELEASED_EVENT_CLASS ||
         event_id == APL_UON_INFO_TIMER_EVENT)
    {
        widgTimerHandler(widg_STOP_INFO_SCREEN_TIMER, mod_id);
        AmanExecRequestStop(mod_id);
        AlibAcknowledgeKey(event_id);/* this is unnecessary for timer_event) */
    }
}

/* ***************************************************************************
 *  widgListSubApp ()
 * ***************************************************************************
 * Description:
 *      This function gets received event_id from the widg module and
 *      performs the select and store list and save operations.
 * Parameters:
 *      t_mmi_event_id
 * Globals:
 *      t_widg_status,t_widg_list_info
 * Return Values:
 *      None.
 * Exceptions:
 *
 * Warnings:
 *
 *****************************************************************************/

GLOBAL_FUNC t_void widgListSubApp (t_mmi_event_id list_event)

{
    t_ucs2_string            * name_ptr;  /*will be used for naming the files */
    t_widg_select_return       select_return;
    t_widg_store_return        store_return;
    e_widg_status              save_return;
    e_widg_status              widglist_result;
    e_widj_status              widj_result;
    t_widg_infotext_info       info_config;
    t_bool                     free_store=FALSE;
    t_widg_gui_status        * gui_status_ptr;
    e_widj_status              widjcopy_result;
    e_mmi_media_type           media_type;

    switch (widg_status.list_state)
    {
    case STOPPED:
        break;
    case WAIT_FOR_START:
        if (list_event ==APL_EXEC_START_EVENT)
        {
            switch(widg_status.list_event)
            {
            case APL_UON_SELECT_EVENT:
            case APL_UON_STORE_EVENT:
                widg_status.list_state=RUNNING;
                widgSetSecSoftkey(&list_info);
                if (NULL_PTR!=list_info.picklist_handle)
                {
                    widgDisplayListScreen (list_info.list_type,
                                           list_info.sec_softkey);
                    widgStartConversion (&list_info,widg_LIST_MODID);
                }
                break;
            case APL_UON_SAVE_EVENT:
                widg_status.list_state=RUNNING;
                widgSaveMedia(save_data_ptr);
                break;
            default:
                break;
            }
        }
        if (list_event ==APL_EXEC_STOP_EVENT)
        {
            widg_status.list_state = STOPPED;
            widg_status.list_substate=widg_LIST_IDLE;
            widglist_result = widg_FAILURE;
            widgStopListHandler(&list_info,widg_LIST_MODID);
        }
        break;

    case RUNNING:
    case SUSPENDED:
        if (EVENT_CLASS(list_event)==APL_EXEC_EVENT_CLASS)
        {
            switch (list_event)
            {
            case APL_EXEC_STOP_EVENT:
                widg_status.list_state = STOPPED;
                widg_status.list_substate=widg_LIST_IDLE;
                widgStopListHandler(&list_info,widg_LIST_MODID); /*think also for wait_for_timer case */
                break;
            case APL_EXEC_SUSPEND_EVENT:
                widg_status.list_state = SUSPENDED;
                widgSuspendList(&list_info,widg_status.list_substate,widg_LIST_MODID);
                break;
            case APL_EXEC_RESUME_EVENT:
                widg_status.list_state = RUNNING;
                widgResumeList(&list_info,widg_status.list_substate,widg_LIST_MODID);
                break;
            default:
                break;
            }
        }

        switch (widg_status.list_substate)
        {
        case widg_SELECT_LIST:
        case widg_SELECT_RADIO_LIST:
        case widg_STORE_LIST:
            widgListEventHandler (list_event, &list_info,
                                  &widglist_result,
                                  widg_LIST_MODID);
            break;

        case widg_SELECT_WAIT_CONV_RETURN:
            if (EVENT_CLASS(list_event)==APL_KEY_DEPRESSED_EVENT_CLASS ||
                EVENT_CLASS(list_event)== APL_KEY_CONTINUED_EVENT_CLASS ||
                EVENT_CLASS(list_event)==APL_KEY_RELEASED_EVENT_CLASS ||
                list_event== APL_LIBRARY_SCROLL_TIMER_EVENT)
            {
                gui_status_ptr=widgGuiWaitEventHandler(list_event,&list_info,widg_LIST_MODID);

                switch(gui_status_ptr->gui_result)
                {
                case widg_GUI_USER_QUIT:
                    widglist_result=widg_USER_QUIT;
                    widgDesktopRestart(widg_LIST_MODID);
                    break;
                case widg_GUI_USER_EXITED:
                    widglist_result=widg_USER_EXITED;
                    AmanExecRequestStop(widg_LIST_MODID);
                    break;
                default:
                    break;
                }
            }
            if (list_event==SPT_widj_MEDIA_CONVERSION_EVENT)
            {
                widglist_result=widg_OK;
                widjComplete(list_info.conv_media_id);
                AmanExecRequestStop(widg_LIST_MODID);
            }
            break;

        case widg_SELECT_WAIT_CONV_PREVIEW:
        case widg_STORE_WAIT_CONV_PREVIEW:
            widgWaitEventHandler (list_event, &list_info,
                                  &widglist_result,
                                  widg_LIST_MODID);
            break;

        case widg_STORE_SAVE_AS_EDIT:

          gui_status_ptr = widgGuiEditEventHandler (list_event,
                                                 &list_info,
                                                 save_alpha_handle);

          if(gui_status_ptr->gui_result==widg_GUI_ITEM_SELECTED)
            {
              t_widj_media_id    media_id;
              t_widj_media_id    store_media_id;
              t_word             index;
   /*           t_word             store_data_length;*/

               switch(gui_status_ptr->selected)
               {
                  case SLPK_SAVE_TEXT:

                  name_ptr->start = list_info.new_name_ptr->start;
                  name_ptr->length = list_info.new_name_ptr->length;

                  index =  list_info.widj_index;

                  store_media_id =list_info.widjlist_ptr[index].media_id;

  /*              store_data_length=list_info.widjlist_ptr[index].media_data_size; */

                  media_id = widjFindMediaObject (name_ptr);

                  if(media_id ==widj_NULL_MEDIA_ID)
                   {
                       /*            widjcheck_result = widjCheckMediaObjectStore(store_media_id,
                                     store_data_length);*/

                       /*          switch (widjcheck_result)
                       {
                       case widj_OK:*/

                       widjcopy_result = widjCopyMediaObject(store_media_id,
                                                             name_ptr);
                       if (widjcopy_result==widj_OK)
                       {
                           widg_status.list_substate= widg_STORE_SAVED_CNF;

                           info_config.new_name_ptr=list_info.new_name_ptr;
                           info_config.line2=SLPK_SAVED_TEXT;

                           list_info.stored_ptr[index]=TRUE;

                           list_info.number_of_stored++;

                           list_info.picklist_ptr[index].check_state=AGLST_ITEM_STATE_STORED;

                           list_info.selected_index++;

                           if (list_info.selected_index>list_info.picklist_length)
                           {
                               list_info.selected_index= list_info.picklist_length;
                           }

                           if (list_info.picklist_handle!=NULL_PTR)
                           {
                               AglstResetList (list_info.picklist_handle,
                                               AGLST_ITEM_CHANGED,
                                               list_info.number_of_media,
                                               list_info.selected_index,
                                               list_info.picklist_ptr);
                           }
                           widgSetSecSoftkey (&list_info); /* primsoftkey is enabled or disabled on return from info state.*/
                       }

                       else
                       {
                           widg_status.list_substate= widg_STORE_SAVE_FAILED;

                           info_config.new_name_ptr->start=list_info.new_name_ptr->start;
                           info_config.new_name_ptr->length=list_info.new_name_ptr->length;
                           info_config.line2=SLPK_SAVE_FAILED_TEXT;

                           list_info.new_name_ptr->start=list_info.old_name_ptr->start;
                           list_info.new_name_ptr->length=list_info.old_name_ptr->length;
                           list_info.stored_ptr[index]=FALSE;
                       }
                   }

                  /*           case widj_NO_FREE_SPACE:

                     widg_status.list_substate = widg_STORE_FREE_STORE_QUERY;

                     widgDisplayQueryScreen(&info_config,widg_STORE_FREE_STORE_QUERY,widg_LIST_MODID);

                     break;

                     default:
                     break;
                     } */

                  else
                  {
                      info_config.new_name_ptr->start=list_info.new_name_ptr->start;
                      info_config.new_name_ptr->length=list_info.new_name_ptr->length;

                      list_info.new_name_ptr->start=list_info.old_name_ptr->start;
                      list_info.new_name_ptr->length=list_info.old_name_ptr->length;

                      widg_status.list_substate= widg_STORE_ALREADY_EXISTS;
                      info_config.line2=SLPK_ALREADY_EXISTS_TEXT;
                      list_info.stored_ptr[index]=FALSE;

                  }

                  widgCreateInfoHandle(&info_config,
                                       widg_status.list_substate,
                                       widg_LIST_MODID);
                  widg_status.list_state=WAIT_FOR_TIMER;

                  break;

               case SLPK_CANCEL_TEXT:
                   widg_status.list_substate= widg_STORE_LIST;

                   if(save_alpha_handle!=NULL_PTR)
                   {
                       AgeditAlphaDestroy(&save_alpha_handle);
                   }

                   if (list_info.picklist_handle!=NULL_PTR)
                   {
                       AglstDisplay(list_info.picklist_handle);
                   }

                   list_info.new_name_ptr->start=list_info.old_name_ptr->start;
                   list_info.new_name_ptr->length=list_info.old_name_ptr->length;
                   list_info.stored_ptr[index]=FALSE;

                   break;

               default:
                   break;
               }
            }
          break;

        case widg_STORE_FREE_STORE_QUERY:
        case widg_SAVE_FREE_STORE_QUERY:
            free_store=widgGuiQueryEventHandler (list_event,
                                                 &list_info,
                                                 &widglist_result,
                                                 widg_LIST_MODID);
            if( free_store==TRUE)
            {
                AevntSendEvent(APL_UON_MANAGE_EVENT,
                              widg_LIST_MODID,
                               widg_MANAGER_MODID,
                               0,
                               NULL_PTR);
            }

            /* start acus media_folders here list_substates are set in suspendlist fnc.*/
            break;

        default: /* widg_SAVE_MEDIA: No specific screen is to be displayed, waiting for manager, suspended*/
            break;
        }
        break;

    case WAIT_FOR_TIMER:
        if(list_event == APL_EXEC_STOP_EVENT)
        {
            widg_status.list_state=STOPPED;
            widg_status.list_substate=widg_LIST_IDLE;
            widgTimerHandler(widg_STOP_INFO_SCREEN_TIMER, widg_LIST_MODID);
            widgStopListHandler(&list_info, widg_LIST_MODID);
        }
        switch(widg_status.list_substate)
        {
        case widg_SELECT_PREV_UNAV_RETURN: /*the widglist_result should have been assigned when this pnt is reached */
        case widg_SELECT_NO_OBJECTS_FOUND:
            widgStopSubappEventHandler(list_event,widg_LIST_MODID);
            break;

        case widg_SAVE_SAVE_FAILED:
        case widg_SAVE_READONLY:
        case widg_SAVE_SAVED_CNF:
        case widg_SELECT_PREV_UNAVAILABLE:
        case widg_STORE_PREV_UNAVAILABLE:
        case widg_STORE_SAVED_CNF:
        case widg_STORE_READONLY:
        case widg_STORE_SAVE_FAILED:
            widgInfoEventHandler (&list_info,list_event,widg_LIST_MODID);
            break;

        case widg_STORE_ALREADY_EXISTS:   /*return to re-edit*/
            if ( EVENT_CLASS(list_event)==APL_KEY_DEPRESSED_EVENT_CLASS ||
                 EVENT_CLASS(list_event)== APL_KEY_CONTINUED_EVENT_CLASS ||
                 EVENT_CLASS(list_event)==APL_KEY_RELEASED_EVENT_CLASS ||
                 list_event == APL_UON_INFO_TIMER_EVENT)
            {
                widgTimerHandler(widg_STOP_INFO_SCREEN_TIMER, widg_LIST_MODID);

                AlibAcknowledgeKey(list_event); /* this is unnecessary for timer_event) */

                widg_status.list_substate=widg_STORE_SAVE_AS_EDIT; /*widgStateHandler */

                if (rename_alpha_handle!=NULL_PTR)
                {
                    AlibClearArea(ALIB_CLEAR_MAINAREA);
                    AlibClearArea(ALIB_CLEAR_SOFTAREA);
                    AgeditAlphaDisplay(rename_alpha_handle);
                }
            }
            break;

        default:
            break;
        }
        break;
    default:
        break;
    }
}
/* ***************************************************************************
 *  widgPreviewSubApp ()
 * ***************************************************************************
 * Description:
 *
 *
 * Parameters:
 *
 * Globals:
 *
 * Return Values:
 *
 * Exceptions:
 *
 * Warnings:
 *
 */

GLOBAL_FUNC t_void widgPreviewSubApp (t_mmi_event_id   event_id)
{
    e_widj_status          widj_result;
    t_widg_infotext_info  info_config;

    if (event_id == APL_EXEC_STOP_EVENT)
    {
        widg_status.preview_substate = widg_PREVIEW_IDLE;
        widg_status.preview_state = STOPPED;
        widgTimerHandler(widg_STOP_INFO_SCREEN_TIMER, widg_PREVIEW_MODID);
        /*Timer handle will discard it if timer_handle is null */
    }
    switch(widg_status.preview_state)
    {
    case STOPPED:
        break;
    case WAIT_FOR_START:
        if (event_id == APL_EXEC_START_EVENT)
        {
            t_widj_media_id   media_id;
            t_mmi_module_id   mod_id;
            widg_status.preview_state=RUNNING;
            mod_id =widg_status.playing_modid;
            switch(mod_id)
            {
            case widg_LIST_MODID:
                media_id = list_info.conv_media_id;
                break;
            case widg_MANAGER_MODID:
                media_id = mnglist_info.conv_media_id;
                break;
            default:
                break;
            }
            widj_result=widjPlayMediaObject(media_id,&play_opts,mod_id);
            if(widj_result!=widj_OK)
            {
                widg_status.preview_substate ==widg_PREVIEW_PREV_UNAVAILABLE;

                info_config.line1=SLPK_PREVIEW_UNAVAILABLE_1_TEXT;
                info_config.line2=SLPK_PREVIEW_UNAVAILABLE_2_TEXT;

                widgCreateInfoHandle(&info_config,
                                     widg_status.list_substate,widg_LIST_MODID);
                widg_status.list_state=WAIT_FOR_TIMER;
            }
            widg_status.preview_substate ==widg_PREVIEW_ACTIVE;
        }
        break;
    case SUSPENDED:
    case RUNNING:
        if ( widg_status.preview_substate ==widg_PREVIEW_ACTIVE &&
             (EVENT_CLASS(event_id)==APL_KEY_DEPRESSED_EVENT_CLASS ||
              EVENT_CLASS(event_id)== APL_KEY_CONTINUED_EVENT_CLASS ||
              EVENT_CLASS(event_id)==APL_KEY_RELEASED_EVENT_CLASS ||
              event_id == APL_EXEC_SUSPEND_EVENT ))
        {
            widg_status.preview_substate= widg_PREVIEW_IDLE;
            if (event_id!=APL_EXEC_SUSPEND_EVENT)
            {
                AlibAcknowledgeKey(event_id);
            }
            AmanExecRequestStop(widg_PREVIEW_MODID);
        }
        if ( widg_status.preview_substate ==widg_PREVIEW_ACTIVE &&
             event_id == APL_EXEC_STOP_EVENT )
        {
            widg_status.preview_substate= widg_PREVIEW_IDLE;
            widg_status.preview_state=STOPPED;
        }
        if(event_id==SPT_widj_MEDIA_PLAY_EVENT)
        {

        }
        break;

    case WAIT_FOR_TIMER:
        if ( widg_status.preview_substate ==widg_PREVIEW_PREV_UNAVAILABLE &&
             (EVENT_CLASS(event_id)==APL_KEY_DEPRESSED_EVENT_CLASS ||
              EVENT_CLASS(event_id)== APL_KEY_CONTINUED_EVENT_CLASS ||
              EVENT_CLASS(event_id)==APL_KEY_RELEASED_EVENT_CLASS ))
        {
            widgTimerHandler(widg_STOP_INFO_SCREEN_TIMER, widg_PREVIEW_MODID);
            AlibAcknowledgeKey(event_id);
            widg_status.preview_substate=widg_PREVIEW_IDLE;
            AmanExecRequestStop(widg_PREVIEW_MODID);
        }
        if(widg_status.preview_substate ==widg_PREVIEW_PREV_UNAVAILABLE &&
           event_id == APL_EXEC_STOP_EVENT )
        {
            widgTimerHandler(widg_STOP_INFO_SCREEN_TIMER, widg_PREVIEW_MODID);
            widg_status.preview_substate=widg_PREVIEW_IDLE;
        }
        break;
    default:
        break;
    }
}

/* ***************************************************************************
 *  widgManageReturn ()
 * **************************************************************************
 * Description:
 *
 *
 * Parameters:
 *
 * Globals:
 *
 * Return Values:
 *
 * Exceptions:
 *
 * Warnings:
 *
 */

LOCAL_FUNC t_void widgManageReturn (e_widg_status manage_status)

{
    t_widg_manage_return  manage_return;

    manage_return.widg_status =manage_status;

    AevntSendEvent( APL_UON_SAVE_RETURN_EVENT,
                    widg_MANAGER_MODID,
                    widg_status.mng_src_modid,
                    sizeof(t_widg_manage_return),  /*check if it should be t_widg_save_return*/
                    &manage_return);
}

/* ***************************************************************************
 *  widgManagerSubApp ()
 * ***************************************************************************
 * Description:
 *
 *
 * Parameters:
 *
 * Globals:
 *
 * Return Values:
 *
 * Exceptions:
 *
 * Warnings:
 *
 */

GLOBAL_FUNC t_void widgManagerSubApp (t_mmi_event_id mng_event)

{
    t_bool                delete_media=FALSE;
    t_bool                delete_all_media=FALSE;
    t_widg_gui_status   * gui_status_ptr;
    t_widj_media_id       cur_media_id;
    t_word                cur_index;
    t_word                list_index;
    t_word                i;
    e_widj_status         widjinfo_result;
    e_widj_status         widjdelete_result;
    e_widj_status         widjcopy_result;
    e_widj_status         widjrename_result;
    t_bool                read_only;
    t_bool                in_use;
    t_ucs2*               media_name_ptr;
    t_word                media_name_length;
    t_widj_media_id       media_id;
    t_widj_media_id       rename_media_id;
    t_widg_infotext_info  info_config;
    t_ucs2              * name_ptr;
    e_mmi_media_type      media_type;

    switch (widg_status.manager_state)
    {
    case STOPPED:
        break;

    case WAIT_FOR_START:
        if (mng_event ==APL_EXEC_START_EVENT)
        {
            widg_status.manager_state=RUNNING;
            if (manager_menu_handle!=NULL_PTR)
            {
                AgmenuDisplayMenu(manager_menu_handle);
            }
        }

        if (mng_event ==APL_EXEC_STOP_EVENT)
        {
            widg_status.manager_state = STOPPED; /* it should already be stopped*/

            widg_status.manager_substate=widg_MANAGER_IDLE;

            widgmanage_result=widg_FAILURE;

            widgStopListHandler(&mnglist_info,widg_MANAGER_MODID);
        }
        break;

    case RUNNING:
    case SUSPENDED:

        if (EVENT_CLASS(mng_event)==APL_EXEC_EVENT_CLASS)
        {
            switch (mng_event)
            {
            case APL_EXEC_STOP_EVENT:
                widg_status.manager_state = STOPPED;

                widg_status.manager_substate=widg_MANAGER_IDLE;

                widgStopListHandler(&mnglist_info,
                                    widg_MANAGER_MODID);
                break;

            case APL_EXEC_SUSPEND_EVENT:
                widg_status.manager_state = SUSPENDED;

                widgSuspendList(&mnglist_info,
                                widg_status.manager_substate,
                                widg_MANAGER_MODID);
                break;

            case APL_EXEC_RESUME_EVENT:
                widg_status.manager_state = RUNNING;

                widgResumeList(&mnglist_info,
                               widg_status.manager_substate,
                               widg_MANAGER_MODID);
                break;

            default:
                break;
            }
        }

        switch (widg_status.manager_substate)
        {
        case widg_MANAGER_MENU_STATE:

            if (EVENT_CLASS(mng_event)==APL_KEY_DEPRESSED_EVENT_CLASS ||
                EVENT_CLASS(mng_event)== APL_KEY_CONTINUED_EVENT_CLASS ||
                EVENT_CLASS(mng_event)==APL_KEY_RELEASED_EVENT_CLASS )
            {
/* are there any scroll timer for menu gui object ??*/
                t_widg_gui_status        * gui_status_ptr;

                gui_status_ptr=widgGuiMenuEventHandler(mng_event);

                if(gui_status_ptr->gui_result==widg_GUI_ITEM_SELECTED)
                {
                    widgCreateManageList(gui_status_ptr->selected);
                }
            }
            /*since there may be no other events other than the key events
              above if cond. can be removed. */

            break;

        case widg_MANAGER_LIST:/*consider popup menu in this state */

            widgListEventHandler (mng_event,
                                  &mnglist_info,
                                  &widglist_result,
                                  widg_LIST_MODID);
            break;

        case widg_MANAGER_DELETE_QUERY:
            /*widj conversion must have been stopped before entering into this state*/

            delete_media = widgGuiQueryEventHandler (mng_event, &mnglist_info,
                                                     &widgmanage_result,
                                                     widg_MANAGER_MODID);
            if (delete_media ==TRUE)
            {
                cur_index = mnglist_info.widj_index;
                cur_media_id=mnglist_info.widjlist_ptr[cur_index].media_id;

                widjinfo_result = widjGetMediaObjectInfo(cur_media_id,
                                                         NULL_PTR,
                                                         NULL_PTR,
                                                         NULL_PTR,
                                                         NULL_PTR,
                                                         NULL_PTR,
                                                         &read_only,
                                                         &in_use);

                media_name_ptr = mnglist_info.widjlist_ptr[cur_index].media_name;
                media_name_length = mnglist_info.widjlist_ptr[cur_index].media_name_length;

                if(widjinfo_result==widj_OK && read_only==TRUE)
                {
                    widg_status.manager_substate=widg_MANAGER_DELETE_READONLY;

                    if(info_config.old_name_ptr==NULL_PTR)
                    {
                        info_config.old_name_ptr=(t_ucs2_string*) our_malloc(sizeof(t_ucs2_string));
                    }

                    info_config.old_name_ptr->start=media_name_ptr;
                    info_config.old_name_ptr->length=media_name_length;
                    info_config.line2=SLPK_IS_READ_ONLY_TEXT;
                }

                if (widjinfo_result==widj_OK && in_use==FALSE && read_only==FALSE)
                 {
                    widjdelete_result =widjDeleteMediaObject(cur_media_id);

                    if (widjdelete_result==widj_OK)
                    {
                        mnglist_info.number_of_media--;

                        /* number_of_deleted is not updated here,
                         because no number will be shown*/

                        if(mnglist_info.number_of_media!=0)
                        {
                            widg_status.manager_substate=widg_MANAGER_DELETED_CNF;

                            if(info_config.old_name_ptr==NULL_PTR)
                            {
                                info_config.old_name_ptr=(t_ucs2_string*) our_malloc(sizeof(t_ucs2_string));
                            }

                            info_config.old_name_ptr->start= media_name_ptr;
                            info_config.old_name_ptr->length=media_name_length;
                            info_config.line2=SLPK_DELETED_TEXT;

                            if (mnglist_info.selected_index==mnglist_info.picklist_length)
                            {
                              mnglist_info.selected_index--;
                              mnglist_info.widj_index--;
                            }

                            else
                            {
                              mnglist_info.selected_index++;
                              mnglist_info.widj_index++;
                            }

                            mnglist_info.picklist_length--;
                            mnglist_info.widjlist_length--;

                            if(mnglist_info.picklist_handle!=NULL_PTR)
                            {
                                AglstResetList (mnglist_info.picklist_handle,
                                                AGLST_ITEM_DELETED,
                                                mnglist_info.number_of_media,
                                                mnglist_info.selected_index,
                                                mnglist_info.picklist_ptr);
                            }

                            widgSetSecSoftkey (&mnglist_info);
                            widgStartConversion (&mnglist_info,
                                                 widg_MANAGER_MODID);
                        }
                        else
                        {
                            widgmanage_result=widg_OK;
                            widg_status.manager_substate=widg_MANAGER_ALL_DELETED_CNF;
                            info_config.line1=SLPK_ALL_TEXT;

                            if(mnglist_info.title_ptr!=NULL_PTR)
                            {
                                if(info_config.new_name_ptr==NULL_PTR)
                                {
                                    info_config.new_name_ptr=(t_ucs2_string*) our_malloc(sizeof(t_ucs2_string));
                                }

                                info_config.new_name_ptr->start=mnglist_info.title_ptr->start;
                                info_config.new_name_ptr->length=mnglist_info.title_ptr->length;
                            }

                            media_type = mnglist_info.widjlist_ptr[mnglist_info.selected_index].media_class.media_type;

                            if(mnglist_info.title_ptr==NULL_PTR && media_type==MMI_AUDIO)
                            {
                                info_config.line2=SLPK_MELODIES_TEXT;
                                if( info_config.new_name_ptr!=NULL_PTR)
                                {
                                    our_free(info_config.new_name_ptr);
                                    info_config.new_name_ptr=NULL_PTR;
                                }
                            }

                            if(mnglist_info.title_ptr==NULL_PTR && media_type!=MMI_AUDIO)
                            {
                                info_config.line2=SLPK_PICTURES_TEXT;

                                if( info_config.new_name_ptr!=NULL_PTR)
                                {
                                    our_free(info_config.new_name_ptr);
                                    info_config.new_name_ptr=NULL_PTR;
                                }

                                /*          info_config.line2=SLPK_OBJECTS_TEXT; */
                            }

                            info_config.line3=SLPK_DELETED_TEXT;
                        }
                    }

                    else
                    {
                        widg_status.manager_substate=widg_MANAGER_DELETE_FAILED;

                        if(info_config.old_name_ptr==NULL_PTR)
                        {
                            info_config.old_name_ptr=(t_ucs2_string*) our_malloc(sizeof(t_ucs2_string));
                        }

                        info_config.old_name_ptr->start=media_name_ptr;
                        info_config.old_name_ptr->length=media_name_length;
                        info_config.line2=SLPK_DELETE_FAILED_TEXT;
                    }

                    if(mnglist_info.old_name_ptr==NULL_PTR)
                    {
                        mnglist_info.old_name_ptr=(t_ucs2_string*) our_malloc(sizeof(t_ucs2_string));
                    }

                    mnglist_info.old_name_ptr->start=media_name_ptr;
                    mnglist_info.old_name_ptr->length=media_name_length;

                    widgCreateInfoHandle(&info_config,
                                         widg_status.manager_substate,
                                         widg_MANAGER_MODID);

                    widg_status.manager_state=WAIT_FOR_TIMER;
                }

          /*Conversion is started and state is set to list state and
             Manage_list is displayed in GuiQueryEventHandler
             if query is not accepted.*/
            }
            break;

        case widg_MANAGER_DELETEALL_QUERY:
            delete_all_media=widgGuiQueryEventHandler (mng_event,
                                                       &mnglist_info,
                                                       &widgmanage_result,
                                                       widg_MANAGER_MODID);
            if (delete_all_media ==TRUE)
            {
                cur_index = mnglist_info.widj_index;

                cur_media_id=mnglist_info.widjlist_ptr[cur_index].media_id;

                for(i=0; i<mnglist_info.widjlist_length; i++)
                {
                    cur_index = mnglist_info.widj_index;
                    cur_media_id=mnglist_info.widjlist_ptr[cur_index].media_id;

                    widjinfo_result = widjGetMediaObjectInfo(cur_media_id,
                                                             NULL_PTR,
                                                             NULL_PTR,
                                                             NULL_PTR,
                                                             NULL_PTR,
                                                             NULL_PTR,
                                                             &read_only,
                                                             &in_use);

                    if (widjinfo_result==widj_OK && read_only!=TRUE && in_use !=TRUE)
                    {
                        widjdelete_result =widjDeleteMediaObject(cur_media_id);

                        if(widjdelete_result==widj_OK);
                        {
                            if (mnglist_info.selected_index==mnglist_info.picklist_length)
                            {
                              mnglist_info.selected_index--;
                              mnglist_info.widj_index--;
                            }

                            else
                            {
                              mnglist_info.selected_index++;
                              mnglist_info.widj_index++;
                            }

                            mnglist_info.picklist_length--;
                            mnglist_info.widjlist_length--;

                            mnglist_info.number_of_media--;
                            mnglist_info.number_of_deleted++;
                        }
                    }
                }

                if(mnglist_info.picklist_length!=0)
                {
                    if(mnglist_info.picklist_handle!=NULL_PTR)
                    {
                        AglstResetList (mnglist_info.picklist_handle,
                                        AGLST_ITEM_DELETED,
                                        mnglist_info.number_of_media,
                                        mnglist_info.selected_index,
                                        mnglist_info.picklist_ptr);
                    }

                    widgSetSecSoftkey (&mnglist_info);
                    widgStartConversion (&mnglist_info,
                                         widg_MANAGER_MODID);
                    widg_status.manager_substate=widg_MANAGER_SOME_DELETED_CNF;
                    info_config.number_of_objects=mnglist_info.number_of_deleted;
                    info_config.line2=mnglist_info.title_id;
                    info_config.line3=SLPK_DELETED_TEXT;
                }
                else
                {
                    widgmanage_result=widg_OK;
                    widg_status.manager_substate=widg_MANAGER_ALL_DELETED_CNF;
                    info_config.line1=SLPK_ALL_TEXT;
                    info_config.line2=mnglist_info.title_id;
                    info_config.line3=SLPK_DELETED_TEXT;
                }
                widgCreateInfoHandle(&info_config,
                                     widg_status.manager_substate,
                                     widg_MANAGER_MODID);
                widg_status.manager_state=WAIT_FOR_TIMER;
            }
/*Conversion is started and state is set to list state and Manage_list is displayed
  in GuiQueryEventHandler if query is not accepted.*/
            break;

        case widg_MANAGER_RENAME_EDIT: /*NO readonly result after this pnt.*/
            gui_status_ptr = widgGuiEditEventHandler (mng_event,
                                                      &mnglist_info,
                                                      rename_alpha_handle);

            if(gui_status_ptr->gui_result==widg_GUI_ITEM_SELECTED)
                {
                    switch(gui_status_ptr->selected)
                    {
                    case SLPK_SAVE_TEXT:

                        media_id = widjFindMediaObject
                            (mnglist_info.new_name_ptr);

                        rename_media_id = mnglist_info.widjlist_ptr
                            [mnglist_info.widj_index].media_id;

                        if(media_id ==widj_NULL_MEDIA_ID)
                        {
                            widjrename_result = widjRenameMediaObject(rename_media_id,
                                                                      mnglist_info.new_name_ptr);
                            if (widjrename_result==widj_OK)
                            {
                                widg_status.manager_substate= widg_MANAGER_RENAMED_TO;

                                if(info_config.old_name_ptr==NULL_PTR)
                                {
                                    info_config.old_name_ptr=(t_ucs2_string*) our_malloc(sizeof(t_ucs2_string));
                                }

                                info_config.old_name_ptr->start=mnglist_info.old_name_ptr->start;
                                info_config.old_name_ptr->length=mnglist_info.old_name_ptr->length;

                                if(info_config.new_name_ptr==NULL_PTR)
                                {
                                    info_config.new_name_ptr=(t_ucs2_string*) our_malloc(sizeof(t_ucs2_string));
                                }

                                info_config.new_name_ptr=mnglist_info.new_name_ptr;
                                info_config.line2=SLPK_RENAMED_TO_TEXT;
                            }
                            else
                            {
                                widg_status.manager_substate= widg_MANAGER_RENAME_FAILED;

                                if(info_config.old_name_ptr==NULL_PTR)
                                {
                                    info_config.old_name_ptr=(t_ucs2_string*) our_malloc(sizeof(t_ucs2_string));
                                }

                                info_config.old_name_ptr->start=mnglist_info.old_name_ptr->start;
                                info_config.old_name_ptr->length=mnglist_info.old_name_ptr->length;
                                info_config.line2=SLPK_RENAME_FAILED_TEXT;

                                mnglist_info.new_name_ptr->start=mnglist_info.old_name_ptr->start;
                                mnglist_info.new_name_ptr->length=mnglist_info.old_name_ptr->length;
                            }
                        }

                        else
                        {
                            widg_status.manager_substate= widg_MANAGER_RENAME_ALREADY_EXISTS;

                            if(info_config.new_name_ptr==NULL_PTR)
                            {
                                info_config.new_name_ptr=(t_ucs2_string*) our_malloc(sizeof(t_ucs2_string));
                            }

                            info_config.new_name_ptr->start=mnglist_info.new_name_ptr->start;
                            info_config.new_name_ptr->length=mnglist_info.new_name_ptr->length;

                            info_config.line2=SLPK_ALREADY_EXISTS_TEXT;

                            mnglist_info.new_name_ptr->start=mnglist_info.old_name_ptr->start;
                            mnglist_info.new_name_ptr->length=mnglist_info.old_name_ptr->length;
                        }

                        widgCreateInfoHandle(&info_config,
                                             widg_status.manager_substate,
                                             widg_MANAGER_MODID);
                        widg_status.manager_state=WAIT_FOR_TIMER;
                        break;

                    case SLPK_CANCEL_TEXT:

                        widg_status.manager_substate= widg_MANAGER_LIST;

                        if (rename_alpha_handle!=NULL_PTR)
                        {
                            AgeditAlphaDestroy(&rename_alpha_handle);
                        }

                        mnglist_info.new_name_ptr->start=mnglist_info.old_name_ptr->start;
                        mnglist_info.new_name_ptr->length=mnglist_info.old_name_ptr->length;

                        widgDisplayListScreen(mnglist_info.list_type,mnglist_info.sec_softkey);

                        break;

                    default:
                        break;
                    }
                }

            break;

        case widg_MANAGER_WAIT_CONV_PREVIEW: /* Please wait converting */
            widgWaitEventHandler (mng_event,
                                  &mnglist_info,
                                  &widgmanage_result,
                                  widg_MANAGER_MODID);
            break;

        case widg_MANAGER_ALL_DELETED_CNF:
            widgStopSubappEventHandler(mng_event,widg_MANAGER_MODID);
            break;
        }
        break;

    case WAIT_FOR_TIMER:
/* info screen states below this pnt. the list will be updated and displayed */
        switch (widg_status.manager_substate)
        {
        case widg_MANAGER_RENAMED_TO:    /* Renamed To, Rename Failed */
        case widg_MANAGER_RENAME_FAILED: /* widj returned invalid media_id */
        case widg_MANAGER_PREV_UNAVAILABLE:
        case widg_MANAGER_DELETED_CNF:
        case widg_MANAGER_DELETE_FAILED:
        case widg_MANAGER_DELETE_READONLY:
        case widg_MANAGER_SOME_DELETED_CNF:
            widgInfoEventHandler (&mnglist_info,mng_event,widg_MANAGER_MODID);
            break;
        case widg_MANAGER_RENAME_ALREADY_EXISTS: /* RETURN to rename-edit */
                if ( EVENT_CLASS(mng_event)==APL_KEY_DEPRESSED_EVENT_CLASS ||
                     EVENT_CLASS(mng_event)== APL_KEY_CONTINUED_EVENT_CLASS ||
                     EVENT_CLASS(mng_event)==APL_KEY_RELEASED_EVENT_CLASS ||
                     mng_event == APL_UON_INFO_TIMER_EVENT)
                {
                    widgTimerHandler(widg_STOP_INFO_SCREEN_TIMER, widg_MANAGER_MODID);

                    AlibAcknowledgeKey(mng_event); /* this is unnecessary for timer_event) */

                    widg_status.manager_substate=widg_MANAGER_RENAME_EDIT; /*widgStateHandler */
                    if(rename_alpha_handle!=NULL_PTR)
                    {
                        AlibClearArea(ALIB_CLEAR_MAINAREA);
                        AlibClearArea(ALIB_CLEAR_SOFTAREA);
                        AgeditAlphaDisplay(rename_alpha_handle);
                    }
                }
                break;

        default:
            break;
        }

        break;

    default:
        break;
    }
}

