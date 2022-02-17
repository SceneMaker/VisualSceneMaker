import * as React from 'react';
import Table from '@mui/material/Table';
import TableBody from '@mui/material/TableBody';
import TableCell from '@mui/material/TableCell';
import TableContainer from '@mui/material/TableContainer';
import TableHead from '@mui/material/TableHead';
import TableRow from '@mui/material/TableRow';
import Paper from '@mui/material/Paper';
import { styled } from '@mui/material/styles';
import { tableCellClasses } from '@mui/material/TableCell';



const StyledTableCell = styled(TableCell)(({ theme }) => ({
    [`&.${tableCellClasses.head}`]: {
        backgroundColor: theme.palette.common.black,
        color: theme.palette.common.white,
    },
    [`&.${tableCellClasses.body}`]: {
        fontSize: 14,
    },
    [`&.${tableCellClasses.root}`]: {
        borderBottom: "none"
    }
}));

const StyledTableRow = styled(TableRow)(({ theme }) => ({
    '&:nth-of-type(odd)': {
        backgroundColor: theme.palette.action.hover,
    },
    // hide last border
    '&:last-child td, &:last-child th': {
        border: 0,
    },
}));

const BasicTable = (props) => {


    return (
        <TableContainer component={Paper}>
            <Table sx={{minWidth: 650}} size="small" aria-label="simple table">
                <TableHead>
                    <StyledTableRow>
                        {
                            props.colNames.map((name, index) => (
                                <StyledTableCell key={index}>{name}</StyledTableCell>
                            ))
                        }
                    </StyledTableRow>
                </TableHead>
                <TableBody>
                    {
                        Object.keys(props.colVals).map((key, index) => (
                            <StyledTableRow
                                key={index}
                                sx={{'&:last-child td, &:last-child th': {border: 0}}}
                            >
                                <StyledTableCell component="th" scope="row">
                                    {key}
                                </StyledTableCell>
                                {/*{props.colVals}*/}
                                {/*<div>*/}
                                    {
                                        props.colVals[key].map((vals, idx) => (
                                            <StyledTableCell align="right"
                                                       key={idx}>
                                                {vals}
                                            </StyledTableCell>
                                        ))
                                    }
                                {/*</div>*/}
                            </StyledTableRow>
                        ))
                    }
                </TableBody>
            </Table>
        </TableContainer>
    );
};

export default BasicTable;