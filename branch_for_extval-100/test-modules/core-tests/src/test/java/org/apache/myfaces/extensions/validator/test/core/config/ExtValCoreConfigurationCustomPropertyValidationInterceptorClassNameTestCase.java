package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.interceptor.PropertyValidationInterceptor;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import java.util.List;
import java.util.Map;

public class ExtValCoreConfigurationCustomPropertyValidationInterceptorClassNameTestCase
        extends ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationCustomPropertyValidationInterceptorClassNameTestCase(
            String name)
    {
        super(name);
    }

    public static class CustomPropertyValidationInterceptor implements
            PropertyValidationInterceptor
    {

        public void afterValidation(FacesContext facesContext,
                                    UIComponent uiComponent, Object convertedObject,
                                    Map<String, Object> properties)
        {

        }

        public boolean beforeValidation(FacesContext facesContext,
                                        UIComponent uiComponent, Object convertedObject,
                                        Map<String, Object> properties)
        {
            return false;
        }

    }

    public static class Custom2PropertyValidationInterceptor implements
            PropertyValidationInterceptor
    {

        public void afterValidation(FacesContext facesContext,
                                    UIComponent uiComponent, Object convertedObject,
                                    Map<String, Object> properties)
        {

        }

        public boolean beforeValidation(FacesContext facesContext,
                                        UIComponent uiComponent, Object convertedObject,
                                        Map<String, Object> properties)
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
                    + ".CUSTOM_PROPERTY_VALIDATION_INTERCEPTOR",
                    CustomPropertyValidationInterceptor.class.getName());
        }
    }

    public void testCustomPropertyValidationInterceptorClassNameDefault()
    {
        assertEquals(1, ExtValContext.getContext()
                .getPropertyValidationInterceptors().size());
    }

    public void testCustomPropertyValidationInterceptorClassNameWebXml()
    {
        List<PropertyValidationInterceptor> data = ExtValContext.getContext()
                .getPropertyValidationInterceptors();
        assertEquals(2, data.size());
        assertEquals(CustomPropertyValidationInterceptor.class.getName(), data
                .get(1).getClass().getName());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationCustomPropertyValidationInterceptorClassNameTestCase.class);
    }

}
