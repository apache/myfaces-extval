package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.interceptor.AbstractValidationInterceptor;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import java.util.Map;

public class ExtValCoreConfigurationInterpretEmptyStringSubmittedValuesAsNullTestCase
        extends ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationInterpretEmptyStringSubmittedValuesAsNullTestCase(
            String name)
    {
        super(name);
    }

    public static class TestValidationInterceptor extends
            AbstractValidationInterceptor
    {

        @Override
        protected MetaDataExtractor getComponentMetaDataExtractor(
                Map<String, Object> properties)
        {
            return null;
        }

        @Override
        protected void initComponent(FacesContext facesContext,
                                     UIComponent uiComponent)
        {

        }

        @Override
        protected void processValidation(FacesContext facesContext,
                                         UIComponent uiComponent, Object convertedObject)
        {

        }

        @Override
        public boolean interpretEmptyStringValuesAsNull()
        {
            return super.interpretEmptyStringValuesAsNull();
        }

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(
                    "javax.faces.INTERPRET_EMPTY_STRING_SUBMITTED_VALUES_AS_NULL",
                    "false");
        }
    }

    @Override
    protected void invokeStartupListeners()
    {
        super.invokeStartupListeners();

        // This is the normal place where the user can intervene in the startup
        // and change the config
        if (needCustomConfig())
        {

            ExtValCoreConfiguration.use(new DefaultExtValCoreConfiguration()
            {

                @Override
                public boolean interpretEmptyStringSubmittedValuesAsNull()
                {
                    return false;
                }

            }, true);
        }
    }

    public void testInterpretEmptyStringSubmittedValuesAsNullDefault()
    {
        TestValidationInterceptor interceptor = new TestValidationInterceptor();
        assertTrue(interceptor.interpretEmptyStringValuesAsNull());
    }

    public void testInterpretEmptyStringSubmittedValuesAsNullWebXml()
    {
        TestValidationInterceptor interceptor = new TestValidationInterceptor();
        assertFalse(interceptor.interpretEmptyStringValuesAsNull());
    }

    public void testInterpretEmptyStringSubmittedValuesAsNullCustomConfig()
    {
        TestValidationInterceptor interceptor = new TestValidationInterceptor();
        assertFalse(interceptor.interpretEmptyStringValuesAsNull());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationInterpretEmptyStringSubmittedValuesAsNullTestCase.class);
    }

}
