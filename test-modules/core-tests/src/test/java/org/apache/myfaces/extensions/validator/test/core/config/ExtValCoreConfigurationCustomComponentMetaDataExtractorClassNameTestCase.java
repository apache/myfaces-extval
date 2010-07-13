package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.DefaultComponentMetaDataExtractorFactory;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.property.DefaultPropertyInformation;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

import javax.faces.context.FacesContext;

public class ExtValCoreConfigurationCustomComponentMetaDataExtractorClassNameTestCase
        extends ExtValCoreConfigurationTestCase
{
    private static final String WEB_XML = "Web.XML";
    private static final String CUSTOM_CONFIG = "Custom config";

    public ExtValCoreConfigurationCustomComponentMetaDataExtractorClassNameTestCase(
            String name)
    {
        super(name);
    }

    public static class CustomMetaDataExtractor implements MetaDataExtractor
    {

        public PropertyInformation extract(FacesContext facesContext,
                                           Object object)
        {
            PropertyInformation result = new DefaultPropertyInformation();
            result.setInformation(PropertyInformationKeys.CUSTOM_PROPERTIES,
                    WEB_XML);
            return result;
        }

    }

    public static class CustomMetaDataExtractor2 implements MetaDataExtractor
    {

        public PropertyInformation extract(FacesContext facesContext,
                                           Object object)
        {
            PropertyInformation result = new DefaultPropertyInformation();
            result.setInformation(PropertyInformationKeys.CUSTOM_PROPERTIES,
                    CUSTOM_CONFIG);
            return result;
        }

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX
                    + ".CUSTOM_COMPONENT_META_DATA_EXTRACTOR",
                    CustomMetaDataExtractor.class.getName());

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
                public String customComponentMetaDataExtractorClassName()
                {

                    return CustomMetaDataExtractor2.class.getName();
                }

            }, true);
        }
    }

    public void testCustomComponentMetaDataExtractorClassNameDefault()
    {
        DefaultComponentMetaDataExtractorFactory factory = new DefaultComponentMetaDataExtractorFactory();
        MetaDataExtractor extractor = factory.create();
        PropertyInformation propInformation = extractor.extract(facesContext,
                new Object());
        // Object isn't allowed so we get an empty propertyInformation obejct
        // back.
        // The web.xml and custom config sets an extractor that enters something
        // here.
        assertNull(propInformation
                .getInformation(PropertyInformationKeys.CUSTOM_PROPERTIES));
    }

    public void testCustomComponentMetaDataExtractorClassNameWebXml()
    {
        DefaultComponentMetaDataExtractorFactory factory = new DefaultComponentMetaDataExtractorFactory();
        MetaDataExtractor extractor = factory.create();
        PropertyInformation propInformation = extractor.extract(facesContext,
                new Object());
        assertEquals(WEB_XML, propInformation
                .getInformation(PropertyInformationKeys.CUSTOM_PROPERTIES));
    }

    public void testCustomComponentMetaDataExtractorClassNameCustomConfig()
    {
        DefaultComponentMetaDataExtractorFactory factory = new DefaultComponentMetaDataExtractorFactory();
        MetaDataExtractor extractor = factory.create();
        PropertyInformation propInformation = extractor.extract(facesContext,
                new Object());
        assertEquals(CUSTOM_CONFIG, propInformation
                .getInformation(PropertyInformationKeys.CUSTOM_PROPERTIES));
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationCustomComponentMetaDataExtractorClassNameTestCase.class);
    }

}
