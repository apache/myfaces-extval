package org.apache.myfaces.extensions.validator.test.util.model;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.validator.ValidatorException;

import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.validation.strategy.AbstractAnnotationValidationStrategy;

public class CustomAnnotationStrategy extends
        AbstractAnnotationValidationStrategy<CustomAnnotation>
{

    @Override
    protected String getValidationErrorMsgKey(CustomAnnotation annotation)
    {
        // We don't have to do anything here, The tests in ExtValUtilsTestCase just deals with extraction.
        return null;
    }

    @Override
    protected void processValidation(FacesContext facesContext,
                                     UIComponent uiComponent, MetaDataEntry metaDataEntry,
                                     Object convertedObject) throws ValidatorException
    {
        return;
        // We don't have to do anything here, The tests in ExtValUtilsTestCase just deals with extraction.

    }

}
