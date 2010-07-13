package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.interceptor.ValidationExceptionInterceptor;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

import javax.faces.component.UIComponent;
import javax.faces.validator.ValidatorException;
import java.util.List;

public class ExtValCoreConfigurationCustomValidationExceptionInterceptorClassNameTestCase
        extends ExtValCoreConfigurationTestCase
{

    public ExtValCoreConfigurationCustomValidationExceptionInterceptorClassNameTestCase(
            String name)
    {
        super(name);
    }

    public static class CustomValidationExceptionInterceptor implements
            ValidationExceptionInterceptor
    {

        public boolean afterThrowing(UIComponent uiComponent,
                                     MetaDataEntry metaDataEntry, Object convertedObject,
                                     ValidatorException validatorException,
                                     ValidationStrategy validatorExceptionSource)
        {
            return false;
        }

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX
                    + ".CUSTOM_VALIDATION_EXCEPTION_INTERCEPTOR",
                    CustomValidationExceptionInterceptor.class.getName());

        }
    }

    public void testCustomValidationExceptionInterceptorClassNameDefault()
    {
        assertEquals(2, ExtValContext.getContext()
                .getValidationExceptionInterceptors().size());
    }

    public void testCustomValidationExceptionInterceptorClassNameWebXml()
    {
        List<ValidationExceptionInterceptor> data = ExtValContext.getContext()
                .getValidationExceptionInterceptors();
        assertEquals(3, data.size());
        assertEquals(CustomValidationExceptionInterceptor.class.getName(), data
                .get(2).getClass().getName());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationCustomValidationExceptionInterceptorClassNameTestCase.class);
    }

}
