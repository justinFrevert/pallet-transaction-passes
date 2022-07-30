use crate::{mock::*, CheckTxPass};

use sp_runtime::traits::SignedExtension;
use frame_support::{assert_ok, assert_noop, unsigned::TransactionValidityError, pallet_prelude::InvalidTransaction};
use frame_support::weights::DispatchInfo;
use codec::Encode;

pub const ALICE: u64 = 1;

#[test]
fn it_creates_hourly_pass() {
	new_test_ext().execute_with(|| {
		// Dispatch a signed extrinsic.
		assert_ok!(TxPass::create_chain_hourly_pass(Origin::signed(ALICE), ALICE));
	});
}


#[test]
fn it_invalidates_txes_if_no_pass_owned() {
	new_test_ext().execute_with(|| {

		let call: &<Test as frame_system::Config>::Call =
		&Call::System(frame_system::Call::remark { remark: vec![1] });

		let remark = "hello".encode();
		assert_ok!(System::remark(Origin::signed(ALICE), remark));

		assert_noop!(CheckTxPass::<Test>::pre_dispatch(
			CheckTxPass::new(),
			&1,
			call,
			&DispatchInfo { weight: 1, ..Default::default() },
			1,

		), TransactionValidityError::Invalid(InvalidTransaction::Payment));

		assert_noop!(CheckTxPass::<Test>::validate(
			&CheckTxPass::new(),
			&1,
			call,
			&DispatchInfo { weight: 1, ..Default::default() },
			1,

		), TransactionValidityError::Invalid(InvalidTransaction::Payment));
	});
}

#[test]
fn it_validates_txes_if_pass_owned() {
	new_test_ext().execute_with(|| {
		// Given an owned pass
		assert_ok!(TxPass::create_chain_hourly_pass(Origin::signed(ALICE), ALICE));

		let call: &<Test as frame_system::Config>::Call =
		&Call::System(frame_system::Call::remark { remark: vec![1] });

		let remark = "hello".encode();
		assert_ok!(System::remark(Origin::signed(ALICE), remark));

		assert_ok!(CheckTxPass::<Test>::pre_dispatch(
			CheckTxPass::new(),
			&1,
			call,
			&DispatchInfo { weight: 1, ..Default::default() },
			1,

		));

		assert_ok!(CheckTxPass::<Test>::validate(
			&CheckTxPass::new(),
			&1,
			call,
			&DispatchInfo { weight: 1, ..Default::default() },
			1,
		));
	});
}