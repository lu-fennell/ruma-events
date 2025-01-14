//! Enums for heterogeneous collections of events, exclusive to event types that implement "at
//! most" the trait of the same name.

use serde::Serialize;

pub use super::{all::StateEvent, raw::only as raw};
use crate::{
    call::{
        answer::AnswerEvent, candidates::CandidatesEvent, hangup::HangupEvent, invite::InviteEvent,
    },
    direct::DirectEvent,
    dummy::DummyEvent,
    forwarded_room_key::ForwardedRoomKeyEvent,
    fully_read::FullyReadEvent,
    ignored_user_list::IgnoredUserListEvent,
    key::verification::{
        accept::AcceptEvent, cancel::CancelEvent, key::KeyEvent, mac::MacEvent,
        request::RequestEvent, start::StartEvent,
    },
    presence::PresenceEvent,
    push_rules::PushRulesEvent,
    receipt::ReceiptEvent,
    room::{
        encrypted::EncryptedEvent,
        message::{feedback::FeedbackEvent, MessageEvent},
        redaction::RedactionEvent,
    },
    room_key::RoomKeyEvent,
    room_key_request::RoomKeyRequestEvent,
    sticker::StickerEvent,
    tag::TagEvent,
    typing::TypingEvent,
    CustomEvent, CustomRoomEvent, TryFromRaw,
};

/// A basic event.
#[derive(Clone, Debug, Serialize)]
#[serde(untagged)]
#[allow(clippy::large_enum_variant)]
pub enum Event {
    /// m.direct
    Direct(DirectEvent),

    /// m.dummy
    Dummy(DummyEvent),

    /// m.forwarded_room_key
    ForwardedRoomKey(ForwardedRoomKeyEvent),

    /// m.fully_read
    FullyRead(FullyReadEvent),

    /// m.key.verification.accept
    KeyVerificationAccept(AcceptEvent),

    /// m.key.verification.cancel
    KeyVerificationCancel(CancelEvent),

    /// m.key.verification.key
    KeyVerificationKey(KeyEvent),

    /// m.key.verification.mac
    KeyVerificationMac(MacEvent),

    /// m.key.verification.request
    KeyVerificationRequest(RequestEvent),

    /// m.key.verification.start
    KeyVerificationStart(StartEvent),

    /// m.ignored_user_list
    IgnoredUserList(IgnoredUserListEvent),

    /// m.presence
    Presence(PresenceEvent),

    /// m.push_rules
    PushRules(PushRulesEvent),

    /// m.room_key
    RoomKey(RoomKeyEvent),

    /// m.room_key_request
    RoomKeyRequest(RoomKeyRequestEvent),

    /// m.receipt
    Receipt(ReceiptEvent),

    /// m.tag
    Tag(TagEvent),

    /// m.typing
    Typing(TypingEvent),

    /// Any basic event that is not part of the specification.
    Custom(CustomEvent),
}

/// A room event.
#[derive(Clone, Debug, Serialize)]
#[serde(untagged)]
#[allow(clippy::large_enum_variant)]
pub enum RoomEvent {
    /// m.call.answer
    CallAnswer(AnswerEvent),

    /// m.call.candidates
    CallCandidates(CandidatesEvent),

    /// m.call.hangup
    CallHangup(HangupEvent),

    /// m.call.invite
    CallInvite(InviteEvent),

    /// m.room.encrypted
    RoomEncrypted(EncryptedEvent),

    /// m.room.message
    RoomMessage(MessageEvent),

    /// m.room.message.feedback
    RoomMessageFeedback(FeedbackEvent),

    /// m.room.redaction
    RoomRedaction(RedactionEvent),

    /// m.sticker
    Sticker(StickerEvent),

    /// Any room event that is not part of the specification.
    CustomRoom(CustomRoomEvent),
}

impl TryFromRaw for Event {
    type Raw = raw::Event;
    type Err = String;

    fn try_from_raw(raw: raw::Event) -> Result<Self, (Self::Err, Self::Raw)> {
        use crate::util::try_convert_variant as conv;
        use raw::Event::*;

        match raw {
            Direct(c) => conv(Direct, Event::Direct, c),
            Dummy(c) => conv(Dummy, Event::Dummy, c),
            ForwardedRoomKey(c) => conv(ForwardedRoomKey, Event::ForwardedRoomKey, c),
            FullyRead(c) => conv(FullyRead, Event::FullyRead, c),
            KeyVerificationAccept(c) => {
                conv(KeyVerificationAccept, Event::KeyVerificationAccept, c)
            }
            KeyVerificationCancel(c) => {
                conv(KeyVerificationCancel, Event::KeyVerificationCancel, c)
            }
            KeyVerificationKey(c) => conv(KeyVerificationKey, Event::KeyVerificationKey, c),
            KeyVerificationMac(c) => conv(KeyVerificationMac, Event::KeyVerificationMac, c),
            KeyVerificationRequest(c) => {
                conv(KeyVerificationRequest, Event::KeyVerificationRequest, c)
            }
            KeyVerificationStart(c) => conv(KeyVerificationStart, Event::KeyVerificationStart, c),
            IgnoredUserList(c) => conv(IgnoredUserList, Event::IgnoredUserList, c),
            Presence(c) => conv(Presence, Event::Presence, c),
            PushRules(c) => conv(PushRules, Event::PushRules, c),
            RoomKey(c) => conv(RoomKey, Event::RoomKey, c),
            RoomKeyRequest(c) => conv(RoomKeyRequest, Event::RoomKeyRequest, c),
            Receipt(c) => conv(Receipt, Event::Receipt, c),
            Tag(c) => conv(Tag, Event::Tag, c),
            Typing(c) => conv(Typing, Event::Typing, c),
            Custom(c) => conv(Custom, Event::Custom, c),
        }
    }
}

impl TryFromRaw for RoomEvent {
    type Raw = raw::RoomEvent;
    type Err = String;

    fn try_from_raw(raw: raw::RoomEvent) -> Result<Self, (Self::Err, Self::Raw)> {
        use crate::util::try_convert_variant as conv;
        use raw::RoomEvent::*;

        match raw {
            CallAnswer(c) => conv(CallAnswer, RoomEvent::CallAnswer, c),
            CallCandidates(c) => conv(CallCandidates, RoomEvent::CallCandidates, c),
            CallHangup(c) => conv(CallHangup, RoomEvent::CallHangup, c),
            CallInvite(c) => conv(CallInvite, RoomEvent::CallInvite, c),
            RoomEncrypted(c) => conv(RoomEncrypted, RoomEvent::RoomEncrypted, c),
            RoomMessage(c) => conv(RoomMessage, RoomEvent::RoomMessage, c),
            RoomMessageFeedback(c) => conv(RoomMessageFeedback, RoomEvent::RoomMessageFeedback, c),
            RoomRedaction(c) => conv(RoomRedaction, RoomEvent::RoomRedaction, c),
            Sticker(c) => conv(Sticker, RoomEvent::Sticker, c),
            CustomRoom(c) => conv(CustomRoom, RoomEvent::CustomRoom, c),
        }
    }
}

impl_from_for_enum!(Event, DirectEvent, Direct);
impl_from_for_enum!(Event, DummyEvent, Dummy);
impl_from_for_enum!(Event, ForwardedRoomKeyEvent, ForwardedRoomKey);
impl_from_for_enum!(Event, FullyReadEvent, FullyRead);
impl_from_for_enum!(Event, AcceptEvent, KeyVerificationAccept);
impl_from_for_enum!(Event, CancelEvent, KeyVerificationCancel);
impl_from_for_enum!(Event, KeyEvent, KeyVerificationKey);
impl_from_for_enum!(Event, MacEvent, KeyVerificationMac);
impl_from_for_enum!(Event, RequestEvent, KeyVerificationRequest);
impl_from_for_enum!(Event, StartEvent, KeyVerificationStart);
impl_from_for_enum!(Event, IgnoredUserListEvent, IgnoredUserList);
impl_from_for_enum!(Event, PresenceEvent, Presence);
impl_from_for_enum!(Event, PushRulesEvent, PushRules);
impl_from_for_enum!(Event, ReceiptEvent, Receipt);
impl_from_for_enum!(Event, TagEvent, Tag);
impl_from_for_enum!(Event, TypingEvent, Typing);
impl_from_for_enum!(Event, CustomEvent, Custom);

impl_from_for_enum!(RoomEvent, AnswerEvent, CallAnswer);
impl_from_for_enum!(RoomEvent, CandidatesEvent, CallCandidates);
impl_from_for_enum!(RoomEvent, HangupEvent, CallHangup);
impl_from_for_enum!(RoomEvent, InviteEvent, CallInvite);
impl_from_for_enum!(RoomEvent, EncryptedEvent, RoomEncrypted);
impl_from_for_enum!(RoomEvent, MessageEvent, RoomMessage);
impl_from_for_enum!(RoomEvent, FeedbackEvent, RoomMessageFeedback);
impl_from_for_enum!(RoomEvent, RedactionEvent, RoomRedaction);
impl_from_for_enum!(RoomEvent, StickerEvent, Sticker);
impl_from_for_enum!(RoomEvent, CustomRoomEvent, CustomRoom);
