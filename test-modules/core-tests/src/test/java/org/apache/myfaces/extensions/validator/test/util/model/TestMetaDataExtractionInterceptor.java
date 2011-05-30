package org.apache.myfaces.extensions.validator.test.util.model;

import org.apache.myfaces.extensions.validator.core.interceptor.MetaDataExtractionInterceptor;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.junit.Ignore;

@Ignore
public class TestMetaDataExtractionInterceptor implements
        MetaDataExtractionInterceptor
{
    public static final String PROPERTY_KEY = "JUnit";
    public static final String PROPERTY_VALUE = "From MetaDataExtractionInterceptor";

    public void afterExtracting(PropertyInformation propertyInformation)
    {
        for (MetaDataEntry metaDataEntry : propertyInformation
                .getMetaDataEntries())
        {
            metaDataEntry.setProperty(PROPERTY_KEY, PROPERTY_VALUE);
        }

    }

}
