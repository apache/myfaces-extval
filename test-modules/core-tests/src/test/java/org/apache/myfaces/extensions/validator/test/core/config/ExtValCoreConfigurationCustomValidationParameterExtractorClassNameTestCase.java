package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.validation.parameter.DefaultValidationParameterExtractor;
import org.apache.myfaces.extensions.validator.core.validation.parameter.DefaultValidationParameterExtractorFactory;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ValidationParameterExtractor;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ValidationParameterExtractorFactory;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

import java.lang.annotation.Annotation;
import java.util.List;
import java.util.Map;

public class ExtValCoreConfigurationCustomValidationParameterExtractorClassNameTestCase
        extends ExtValCoreConfigurationTestCase
{

    public ExtValCoreConfigurationCustomValidationParameterExtractorClassNameTestCase(
            String name)
    {
        super(name);
    }

    public static class CustomValidationParameterExtractor implements
            ValidationParameterExtractor
    {

        public Map<Object, List<Object>> extract(Annotation annotation)
        {
            return null;
        }

        public List<Object> extract(Annotation annotation, Object key)
        {
            return null;
        }

        public <T> List<T> extract(Annotation annotation, Object key,
                                   Class<T> valueType)
        {
            return null;
        }

        public <T> T extract(Annotation annotation, Object key,
                             Class<T> valueType, Class valueId)
        {
            return null;
        }

    }

    public static class Custom2ValidationParameterExtractor implements
            ValidationParameterExtractor
    {

        public Map<Object, List<Object>> extract(Annotation annotation)
        {
            return null;
        }

        public List<Object> extract(Annotation annotation, Object key)
        {
            return null;
        }

        public <T> List<T> extract(Annotation annotation, Object key,
                                   Class<T> valueType)
        {
            return null;
        }

        public <T> T extract(Annotation annotation, Object key,
                             Class<T> valueType, Class valueId)
        {
            return null;
        }

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX
                    + ".CUSTOM_VALIDATION_PARAMETER_EXTRACTOR",
                    CustomValidationParameterExtractor.class.getName());
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
                public String customValidationParameterExtractorClassName()
                {
                    return Custom2ValidationParameterExtractor.class.getName();
                }

            }, true);
        }
    }

    public void testCustomValidationParameterExtractorClassNameDefault()
    {
        ValidationParameterExtractorFactory factory = new DefaultValidationParameterExtractorFactory();
        assertEquals(DefaultValidationParameterExtractor.class.getName(),
                factory.create().getClass().getName());

    }

    public void testCustomValidationParameterExtractorClassNameWebXml()
    {
        ValidationParameterExtractorFactory factory = new DefaultValidationParameterExtractorFactory();
        assertEquals(CustomValidationParameterExtractor.class.getName(),
                factory.create().getClass().getName());
    }

    public void testCustomValidationParameterExtractorClassNameCustomConfig()
    {
        ValidationParameterExtractorFactory factory = new DefaultValidationParameterExtractorFactory();
        assertEquals(Custom2ValidationParameterExtractor.class.getName(),
                factory.create().getClass().getName());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationCustomValidationParameterExtractorClassNameTestCase.class);
    }

}
