package org.apache.myfaces.extensions.validator.test.util.model;

import org.apache.myfaces.extensions.validator.core.metadata.CommonMetaDataKeys;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.MetaDataTransformer;

import java.lang.annotation.Annotation;
import java.util.HashMap;
import java.util.Map;

public class CustomAnnotationMetaDataTransformer implements MetaDataTransformer
{

    public Map<String, Object> convertMetaData(MetaDataEntry metaDataEntry)
    {
        Map<String, Object> results = new HashMap<String, Object>();

        Annotation annotation = metaDataEntry.getValue(Annotation.class);

        if (annotation instanceof CustomAnnotation)
        {
            if ((((CustomAnnotation) annotation).required()))
            {
                results.put(CommonMetaDataKeys.REQUIRED, true);
            }
            results.put(
                    TestMetaDataExtractionInterceptor.PROPERTY_KEY,
                    metaDataEntry
                            .getProperty(TestMetaDataExtractionInterceptor.PROPERTY_KEY));
        }
        return results;
    }

}
